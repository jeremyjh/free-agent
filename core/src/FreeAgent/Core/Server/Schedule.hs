{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies                      #-}


module FreeAgent.Core.Server.Schedule
    ( scheduleServer
    ) where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core                           hiding (lookupEvent)
import           FreeAgent.Core.Lenses
import           FreeAgent.Core.Protocol.Schedule         (serverName)
import           FreeAgent.Database.AcidState
import           FreeAgent.Server.ManagedAgent

import           Control.Monad.Reader                     (ask)
import           Control.Monad.State                      (StateT)
import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set

import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.Extras.Timer (Tick (..), TimerRef,
                                                           cancelTimer, sendAfter)

import           Data.Default                             (Default (..))
import           Data.Time.Clock                          (addUTCTime, diffUTCTime)
import           System.Cron


-------------------------
-- Persistence
-- ----------------------
type NextScheduled = (UTCTime, Event)

data SchedulePersist
  = SchedulePersist { _persistEvents        :: Map Key Event
                    , _persistNextScheduled :: Set NextScheduled
                    } deriving (Show, Typeable)

instance Default SchedulePersist where
    def = SchedulePersist mempty Set.empty

data ScheduleState
   = ScheduleState { _stateAcid      :: !(AcidState SchedulePersist)
                   , _stateTickerRef :: Maybe TimerRef
                   , _stateTicking   :: Bool
                   } deriving (Typeable, Generic)

makeFields ''SchedulePersist

makeFields ''ScheduleState

getPersist :: Query SchedulePersist SchedulePersist
getPersist = ask

calcNextScheduled :: UTCTime -> Event -> NextScheduled
calcNextScheduled now event = scheduleFor $ schedRecur event
  where
    --TODO: need a more efficient implementation than brute force -
    --take a look a Quartz CronExpression
    scheduleFor (RecurCron sched _) = untilMatches now
      where untilMatches time'
              | scheduleMatches sched time' = (time', event)
              | otherwise = untilMatches (addMinutes 1 time')
    scheduleFor (RecurInterval ms) =
        let diff = timeIntervalToDiffTime (milliSeconds ms) in
        (addUTCTime diff now, event)
    scheduleFor (OnceAt time') = (time', event)

addMinutes :: Int -> UTCTime -> UTCTime
addMinutes additional time' =
    let minute' = timeIntervalToDiffTime (minutes additional) in
    addUTCTime minute' time'

nextMinute :: MonadIO io => io UTCTime
nextMinute = addMinutes 1 `liftM` getCurrentTime

lookupEvent :: Key -> Query SchedulePersist (Maybe Event)
lookupEvent key' = views events $ Map.lookup key'

allCronEvents :: Query SchedulePersist [(CronSchedule, Key)]
allCronEvents = Map.foldr crons [] <$> view events
  where crons Event{..} acc
          | isCron schedRecur = (cronFrom schedRecur, schedKey) : acc
          | otherwise = acc
          where isCron (RecurCron _ _) = True
                isCron _ = False
                cronFrom (RecurCron cron' _) = cron'
                cronFrom _ = $(err "illegal pattern match")

queryAllEvents :: Query SchedulePersist [Event]
queryAllEvents = views events (snd . unzip . Map.toList)

readyToRun :: UTCTime -> Update SchedulePersist [Event]
readyToRun now = do
    nextScheduled' <- use nextScheduled
    let emptyEvent = (now, Event "" (RecurInterval 0) Never zeroDate False)
        (ready, waiting) = Set.split emptyEvent nextScheduled'
        nextAfter = Set.map (\(_, ev) ->
                                case schedRecur ev of
                                    RecurCron _ _  -> -- Cron is precise to minute
                                        calcNextScheduled (addMinutes 1 now) ev
                                    _ -> calcNextScheduled now ev
                            ) ready
    nextScheduled .= Set.union nextAfter waiting
    return $ snd . unzip $ Set.toList ready

addEvent :: NextScheduled -> Update SchedulePersist ()
addEvent next@(_, event) = do
    events %= Map.insertWith newer (key event) event
    nextScheduled %= Set.insert next
  where newer new old
          | schedModified new > schedModified old = new
          | otherwise = old

removeEvent :: Key -> Update SchedulePersist ()
removeEvent key' = do
    events %= Map.delete key'
    nextScheduled %= Set.filter (\ (_, ev) -> key ev /= key')

disableEvents :: [Key] -> Update SchedulePersist ()
disableEvents keys' =
 do events %= flip ( foldr $ \k es ->
                        Map.update (\e -> Just e {schedDisabled = True}) k es
                   ) keys'
    nextScheduled %= Set.filter (\e -> schedKey (snd e) `notElem` keys')

enableEvents :: UTCTime -> [Key] -> Update SchedulePersist ()
enableEvents now keys' =
 do events' <- use events
    forM_ keys' $ \key' ->
     do let mevent = Map.lookup key' events'
        case mevent of
            Just event' ->
             do let enabled = event' {schedDisabled = False}
                events %= Map.insert key' enabled
                nextScheduled %= Set.insert (calcNextScheduled now enabled)
            Nothing -> return ()

nextScheduledTime :: Query SchedulePersist (Maybe UTCTime)
nextScheduledTime = do
    scheduled <- view nextScheduled
    return $ if Set.null scheduled
             then Nothing
             else let (time',_) = Set.findMin scheduled in Just time'

-- we have to make the splices near the top of the file
$(makeAcidic ''SchedulePersist ['getPersist, 'addEvent, 'lookupEvent
                               , 'allCronEvents, 'queryAllEvents, 'readyToRun, 'nextScheduledTime
                               , 'removeEvent, 'disableEvents, 'enableEvents ])

-- -------Implementation------
-- Implementation
-- ---------------------------

type ScheduleAgent a = StateT ScheduleState Agent a

type instance ProtoT rq ScheduleState a = StateT ScheduleState Agent a

scheduleImpl :: ScheduleImpl ScheduleState
scheduleImpl = ScheduleImpl callScheduleAddEvent' callScheduleEventControl'
                            callScheduleLookupEvent' callScheduleQueryEvents'
                            callScheduleRemoveEvent' castScheduleControl'
  where
    callScheduleAddEvent' (ScheduleAddEvent key' recur retry) =
      do now <- getCurrentTime
         callScheduleAddEvent' (ScheduleAddNewerEvent key' recur retry now)

    callScheduleAddEvent' (ScheduleAddNewerEvent key' recur retry modTime) =
      do let event = Event key' recur retry modTime False
         next <- nextMinute
         () <- update (AddEvent $ calcNextScheduled next event)
         scheduleNextTick

    callScheduleAddEvent' (ScheduleAddEvents events') =
      do next <- nextMinute
         forM_ events' $ \event -> update (AddEvent $ calcNextScheduled next event)
         scheduleNextTick

    callScheduleEventControl' (ScheduleDisableEvents keys') =
        update (DisableEvents keys')

    callScheduleEventControl' (ScheduleEnableEvents keys') =
     do now <- getCurrentTime
        void $ update (EnableEvents now keys')
        scheduleNextTick

    callScheduleLookupEvent' (ScheduleLookupEvent k) =
        query (LookupEvent k)

    callScheduleQueryEvents' _ = query QueryAllEvents

    callScheduleRemoveEvent' cmd@(ScheduleRemoveEvent k) =
        runLogEitherT cmd $ update (RemoveEvent k)

    castScheduleControl' ScheduleStart  =
     do ticking' <- use ticking
        if not ticking'
          then do ticking .= True
                  [qinfo|Starting scheduler.|]
                  tickMe
          else [qinfo|Scheduler already running.|]

    castScheduleControl' ScheduleStop  =
     do ticking' <- use ticking
        if ticking'
          then do [qinfo|Stopping scheduler.|]
                  ticking .= False
                  ticker <- use tickerRef
                  case ticker of
                      Just ref ->
                          liftP $ cancelTimer ref
                      Nothing -> return ()
          else [qinfo|Scheduler already stopped.|]

scheduleServer :: AgentServer
scheduleServer =
    defineServer
        serverName
        initSchedule
        defaultProcess {
            apiHandlers =
            [ registerCall scheduleImpl (Proxy :: Proxy ScheduleRemoveEvent)
            , registerCall scheduleImpl (Proxy :: Proxy ScheduleAddEvent)
            , registerCall scheduleImpl (Proxy :: Proxy ScheduleLookupEvent)
            , registerCall scheduleImpl (Proxy :: Proxy ScheduleQueryEvents)
            , registerCall scheduleImpl (Proxy :: Proxy ScheduleEventControl)
            , registerCast scheduleImpl (Proxy :: Proxy ScheduleControl)
            ],
            infoHandlers =
            [ agentInfoHandler $ \ Tick -> onTick
            ],
            shutdownHandler =
                \ (AgentState _ _ s) _ ->
             do case s ^. tickerRef of
                   Just ref -> cancelTimer ref
                   Nothing -> return ()
                pid <- getSelfPid
                say $ "Schedule server " ++ show pid ++ " shutting down."
        }
  where initSchedule = do
            acid' <- openOrGetDb "agent-schedule" def def
            startNow <- viewConfig initScheduler
            if startNow then
             do [qinfo|Starting scheduler.|]
                tickMe
            else [qinfo|Not starting scheduler.|]
            return $ ScheduleState acid' Nothing startNow

tickMe :: MonadProcess process => process ()
tickMe =
 do pid <- getSelfPid
    send pid Tick

onTick :: ScheduleAgent ()
onTick = do
    now <- getCurrentTime
    events' <- update (ReadyToRun now)
    Right () <- castServ $ ExecuteBatch (map key events')
    scheduleNextTick

scheduleNextTick :: ScheduleAgent ()
scheduleNextTick =
 do ticking' <- use ticking
    when ticking' $
     do mminTime <- query NextScheduledTime
        case mminTime of
            Nothing -> return ()
            Just minTime -> do
                pid <- getSelfPid
                diff <- diffUTCTime minTime <$> getCurrentTime
                tickAt pid diff
    return ()
  where tickAt pid diff
          | diff <= 0 = do
              send pid Tick
              tickerRef .= Nothing
          | otherwise = do
            let interval = diffTimeToTimeInterval diff
            ref <- liftP $ sendAfter interval pid Tick
            tickerRef .= Just ref

deriveSafeStore ''SchedulePersist
