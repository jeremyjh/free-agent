{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies                   #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                               #-}


module FreeAgent.Server.Schedule where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Database.AcidState
import           FreeAgent.Orphans                          ()
import           FreeAgent.Process
import           FreeAgent.Server.Executive                 (ExecuteStored (..))
import           FreeAgent.Server.ManagedAgent

import           Control.Error                              (note)
import           Control.Monad.Reader                       (ask)
import           Control.Monad.State                        (StateT)
import qualified Data.Map.Strict                            as Map
import qualified Data.Set                                   as Set

import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform.Timer (Tick (..),
                                                             TimerRef,
                                                             cancelTimer,
                                                             sendAfter)

import           Data.Attoparsec.Text                       (parseOnly)
import           Data.Binary                                (Binary)
import           Data.Default                               (Default (..))
import           Data.Time.Clock                            (addUTCTime,
                                                             diffUTCTime)
import           System.Cron
import           System.Cron.Parser                         (cronSchedule)

-- ---------Types-------------
-- Types
-- ---------------------------

data Event
  = Event { schedKey      :: !Key
          , schedRecur    :: !ScheduleRecurrence
          , schedRetry    :: !RetryOption
          , schedModified :: !UTCTime
          } deriving (Show, Eq, Typeable, Generic)

data ScheduleRecurrence
  = RecurCron !CronSchedule !Text -- ^ execute when schedule matches
  | RecurInterval !Int           -- ^ execute every n milliseconds
  | OnceAt !UTCTime
  deriving (Show, Eq, Typeable, Generic)

instance Stashable Event where
    key = schedKey

instance Ord Event where
    compare ev1 ev2 = compare (key ev1) (key ev2)

type NextScheduled = (UTCTime, Event)

data RetryOption = Never
                   -- Retry N times at M interval
                 | Fixed Int Int
                   -- Retry N times at exponentially increasing interval
                   -- starting from M
                 | Exponential Int Int
     deriving (Show, Eq, Typeable, Generic)

data ScheduleFail = SCallFailed CallFail
              | EventNotFound Key
              | SDBException !Text
              deriving (Show, Eq, Typeable, Generic)

data SchedulePersist
  = SchedulePersist { persistEvents        :: Map Key Event
                    , persistNextScheduled :: Set NextScheduled
                    } deriving (Show, Typeable)

instance Default SchedulePersist where
    def = SchedulePersist mempty Set.empty

data ScheduleState
   = ScheduleState { stateAcid      :: !(AcidState SchedulePersist)
                   , stateTickerRef :: Maybe TimerRef
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

scheduleLookupEvent :: Key -> Query SchedulePersist (Maybe Event)
scheduleLookupEvent key' = views events $ Map.lookup key'

allCronEvents :: Query SchedulePersist [(CronSchedule, Key)]
allCronEvents = Map.foldr crons [] <$> view events
  where crons Event{..} acc
          | isCron schedRecur = (cronFrom schedRecur, schedKey) : acc
          | otherwise = acc
          where isCron (RecurCron _ _) = True
                isCron _ = False
                cronFrom (RecurCron cron' _) = cron'
                cronFrom _ = $(err "illegal pattern match")

readyToRun :: UTCTime -> Update SchedulePersist (Set (UTCTime, Event))
readyToRun now = do
    nextScheduled' <- use nextScheduled
    let emptyEvent = (now, Event "" (RecurInterval 0) Never zeroDate)
        (ready, waiting) = Set.split emptyEvent nextScheduled'
        nextAfter = Set.map (\(_, ev) ->
                                calcNextScheduled (addMinutes 1 now) ev
                            ) ready
    nextScheduled .= Set.union nextAfter waiting
    return ready

addEvent :: NextScheduled -> Update SchedulePersist ()
addEvent next@(_, event) = do
    events %= Map.insertWith newer (key event) event
    nextScheduled %= Set.insert next
  where newer new old
          | schedModified new > schedModified old = new
          | otherwise = old

scheduleRemoveEvent :: Key -> Update SchedulePersist ()
scheduleRemoveEvent key' = do
    events %= Map.delete key'
    nextScheduled %= Set.filter (\ (_, ev) -> key ev /= key')

nextScheduledTime :: Query SchedulePersist (Maybe UTCTime)
nextScheduledTime = do
    scheduled <- view nextScheduled
    return $ if Set.null scheduled
             then Nothing
             else let (time',_) = Set.findMin scheduled in Just time'

-- we have to make the splices near the top of the file
$(makeAcidic ''SchedulePersist ['getPersist, 'addEvent, 'scheduleLookupEvent
                               , 'allCronEvents, 'readyToRun, 'nextScheduledTime
                               , 'scheduleRemoveEvent ])

-- ---------API---------------
-- API
-- ---------------------------

-- | Advertise a Server on the local peer
schedule :: MonadAgent agent
         => Key -> ScheduleRecurrence -> RetryOption
         -> agent (Either ScheduleFail ())
schedule !key' !recur !retry = do --TODO - why is call not forcing evaluation of NFData Event?
    efail <- callServ (ScheduleAddEvent key' recur retry)
    case efail of
        Right result' -> return result'
        Left failed -> return $ Left (SCallFailed failed)

-- | Advertise a Server on the local peer
unschedule :: MonadAgent agent
              => Key
              -> agent (Either ScheduleFail ())
unschedule key' = do
    efail <- callServ (ScheduleRemoveEvent key')
    case efail of
        Right result' -> return result'
        Left failed -> return $ Left (SCallFailed failed)

lookupEvent :: MonadAgent agent
            => Key -> agent (Either ScheduleFail Event)
lookupEvent key' = do
    emevent <- callServ (ScheduleLookupEvent key')
    case emevent of
        Right mevent -> return $ note (EventNotFound key') mevent
        Left failed -> return $ Left (SCallFailed failed)

-- -------Implementation------
-- Implementation
-- ---------------------------
data ScheduleAddEvent
    = ScheduleAddEvent Key ScheduleRecurrence RetryOption
    | ScheduleAddNewerEvent Key ScheduleRecurrence RetryOption UTCTime
    deriving (Show, Typeable, Generic)

instance Binary ScheduleAddEvent
instance NFData ScheduleAddEvent where rnf = genericRnf

instance ServerCall ScheduleAddEvent (Either ScheduleFail ()) ScheduleState where
    callName _ = serverName
    respond (ScheduleAddEvent key' recur retry) =
     do now <- getCurrentTime
        respond (ScheduleAddNewerEvent key' recur retry now)
    respond cmd@(ScheduleAddNewerEvent key' recur retry time) =
        runLogEitherT cmd $
         do let event = Event key' recur retry time
            () <- update (AddEvent $ calcNextScheduled time event)
            lift scheduleNextTick

deriving instance Generic ScheduleLookupEvent
deriving instance Show ScheduleLookupEvent
instance NFData ScheduleLookupEvent where rnf = genericRnf
instance Binary ScheduleLookupEvent

instance ServerCall ScheduleLookupEvent (Maybe Event) ScheduleState where
    callName _ = serverName
    respond = query

deriving instance Generic ScheduleRemoveEvent
deriving instance Show ScheduleRemoveEvent
instance Binary ScheduleRemoveEvent
instance NFData ScheduleRemoveEvent where rnf = genericRnf

instance ServerCall ScheduleRemoveEvent (Either ScheduleFail ()) ScheduleState where
    callName _ = serverName
    respond cmd = runLogEitherT cmd $ update cmd

serverName :: String
serverName = "agent:schedule"

type ScheduleAgent a = StateT ScheduleState Agent a
type ScheduleAgentET a = EitherT ScheduleFail (StateT ScheduleState Agent) a

scheduleServer :: AgentServer
scheduleServer =
    defineServer
        serverName
        initSchedule
        defaultProcess {
            apiHandlers =
            [ registerCall (Proxy :: Proxy ScheduleRemoveEvent)
            , registerCall (Proxy :: Proxy ScheduleAddEvent)
            , registerCall (Proxy :: Proxy ScheduleLookupEvent)
            ],
            infoHandlers =
            [ agentInfoHandler $ \ Tick -> onTick
            ],
            shutdownHandler =
                \ s _ ->
                case s ^. serverState.tickerRef of
                   Just ref -> cancelTimer ref
                   Nothing -> return ()
        }
  where initSchedule = do
            acid' <- openOrGetDb "agent-schedule" def def
            pid <- getSelfPid
            send pid Tick
            return $ ScheduleState acid' Nothing

onTick :: ScheduleAgent ()
onTick = do
    now <- getCurrentTime
    events' <- update (ReadyToRun now)
    forM_ events' $ \ (_, event') -> do
        Right () <- castServ $ ExecuteStored (key event')
        return ()
    scheduleNextTick

scheduleNextTick :: ScheduleAgent ()
scheduleNextTick = do
    mminTime <- query NextScheduledTime
    case mminTime of
        Nothing -> return ()
        Just minTime -> do
            pid <- getSelfPid
            diff <- diffUTCTime minTime <$> getCurrentTime
            tickAt pid diff
  where tickAt pid diff
          | diff <= 0 = do
              send pid Tick
              tickerRef .= Nothing
          | otherwise = do
            let interval = diffTimeToTimeInterval diff
            ref <- liftProcess $ sendAfter interval pid Tick
            tickerRef .= Just ref

doAddSchedule :: Event -> ScheduleAgentET ()
doAddSchedule event = do
    now <- getCurrentTime
    () <- update (AddEvent $ calcNextScheduled now event)
    lift scheduleNextTick

doRemoveSchedule :: Key -> ScheduleAgentET ()
doRemoveSchedule key' =
    update (ScheduleRemoveEvent key')

cronEvent :: Text -> Either String ScheduleRecurrence
cronEvent format =
    case parseOnly cronSchedule format of
        Right sched -> Right $ RecurCron sched format
        Left msg -> Left msg

instance IsString ScheduleRecurrence where
    fromString format =
        case cronEvent (pack format) of
            Right cron' -> cron'
            _ -> error $ "Unable to parse cron formatted literal: "
                         ++ unpack format


deriveSerializers ''RetryOption

instance Binary ScheduleFail
instance NFData ScheduleFail where rnf = genericRnf

deriveSerializers ''ScheduleRecurrence

instance Binary Event
instance NFData Event where rnf = genericRnf
deriveSafeStore ''Event

deriveSafeStore ''SchedulePersist
