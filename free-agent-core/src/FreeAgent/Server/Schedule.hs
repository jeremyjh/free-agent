{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


module FreeAgent.Server.Schedule where

import           AgentPrelude
import           FreeAgent.Database.AcidState
import           FreeAgent.Lenses
import           FreeAgent.Server.Peer (callServer, CallFail(..))
import           FreeAgent.Server.Executive (executeRegisteredAsync)
import           FreeAgent.Orphans ()
import           FreeAgent.Process
import           FreeAgent.Process.ManagedAgent

import           Control.Monad.Reader (ask)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State (StateT)
import           Control.Error (note)

import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform.Timer
       (Tick(..), cancelTimer, TimerRef, sendAfter)
import Control.DeepSeq.TH (deriveNFData)
import           Data.Default (Default(..))
import           Data.Attoparsec.Text (parseOnly)
import           Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Binary (Binary)
import           System.Cron
import           System.Cron.Parser (cronSchedule)

-- ---------Types-------------
-- Types
-- ---------------------------

data Event
  = Event { schedKey   :: !Key
          , schedRecur :: !ScheduleRecurrence
          , schedRetry :: !RetryOption
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

data ScheduleCommand
  = CmdAddEvent Event
  | CmdRemoveEvent Key
  | CmdFindEvent Key
    deriving (Show, Eq, Typeable, Generic)

data ScheduleFail = SCallFailed CallFail
              | CronParseFail String
              | EventNotFound Key
              | SDBException !Text
              deriving (Show, Eq, Typeable, Generic)

data SchedulePersist
  = SchedulePersist { _persistEvents :: Map Key Event
                    , _persistNextScheduled :: Set NextScheduled
                    } deriving (Show, Typeable)

instance Default SchedulePersist where
    def = SchedulePersist mempty Set.empty

data ScheduleState
   = ScheduleState { _stateAcid    :: !(AcidState SchedulePersist)
                   , _stateTickerRef :: Maybe TimerRef
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

findEventA :: Key -> Query SchedulePersist (Maybe Event)
findEventA key' = views events $ Map.lookup key'

allCronEvents :: Query SchedulePersist [(CronSchedule, Key)]
allCronEvents = view events >>= return . Map.foldr crons []
  where crons event'@Event{..} acc
          | isCron schedRecur == True = (cronFrom schedRecur, schedKey) : acc
          | otherwise = acc
          where isCron (RecurCron _ _) = True
                isCron _ = False
                cronFrom (RecurCron cron' _) = cron'
                cronFrom _ = $(err "illegal pattern match")

readyToRun :: UTCTime -> Update SchedulePersist (Set (UTCTime, Event))
readyToRun now = do
    nextScheduled' <- use nextScheduled
    let splitEvent = (now, Event "" undefined undefined)
        (ready, waiting) = Set.split splitEvent nextScheduled'
        nextAfter = Set.map (\(_, ev) ->
                                calcNextScheduled (addMinutes 1 now) ev
                            ) ready
    nextScheduled .= Set.union nextAfter waiting
    return ready

addEvent :: NextScheduled -> Update SchedulePersist ()
addEvent next@(_, event) = do
    events %= Map.insert (key event) event
    nextScheduled %= Set.insert next

removeEvent :: Key -> Update SchedulePersist ()
removeEvent key' = do
    events %= Map.delete key'
    nextScheduled %= Set.filter (\ (_, ev) -> (key ev) /= key')

nextScheduledTime :: Query SchedulePersist (Maybe UTCTime)
nextScheduledTime = do
    scheduled <- view nextScheduled
    return $ case Set.null scheduled of
        False -> let (time',_) = Set.findMin scheduled
                in Just time'
        True -> Nothing

-- we have to make the splices near the top of the file
$(makeAcidic ''SchedulePersist ['getPersist, 'addEvent, 'findEventA
                               , 'allCronEvents, 'readyToRun, 'nextScheduledTime
                               , 'removeEvent ])

-- ---------API---------------
-- API
-- ---------------------------

-- | Advertise a Server on the local peer
schedule :: MonadProcess m
              => Target -> Event
              -> m (Either ScheduleFail ())
schedule target !event = do --TODO - why is call not forcing evaluation of NFData Event?
    efail <- callServer serverName target (CmdAddEvent event)
    case efail of
        Right result' -> return result'
        Left failed -> return $ Left (SCallFailed failed)

-- | Advertise a Server on the local peer
unschedule :: MonadProcess m
              => Target -> Key
              -> m (Either ScheduleFail ())
unschedule target key' = do
    efail <- callServer serverName target (CmdRemoveEvent key')
    case efail of
        Right result' -> return result'
        Left failed -> return $ Left (SCallFailed failed)

findEvent :: MonadProcess m
          => Target -> Key
          -> m (Either ScheduleFail Event)
findEvent target key' = do
    emevent <- callServer serverName target (CmdFindEvent key')
    case emevent of
        Right mevent -> return $ note (EventNotFound key') mevent
        Left failed -> return $ Left (SCallFailed failed)

-- -------Implementation------
-- Implementation
-- ---------------------------

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
            [ agentRpcHandlerET $ \ cmd ->
                  case cmd of
                      (CmdAddEvent sched) ->
                          doAddSchedule sched
                      (CmdRemoveEvent key') ->
                          doRemoveSchedule key'
                      _ -> $(err "illegal pattern match")
            , agentRpcHandler $ \ (CmdFindEvent key') ->
                  query (FindEventA key')
            ],
            infoHandlers =
            [ agentInfoHandler $ \ Tick -> onTick
            ],
            shutdownHandler =
                \ s _ ->
                case s^.serverState.tickerRef of
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
    forM_ events' $ \ (_, event') ->
        executeRegisteredAsync Local (key event')
    scheduleNextTick

scheduleNextTick :: ScheduleAgent ()
scheduleNextTick = do
    mminTime <- query NextScheduledTime
    case mminTime of
        Nothing -> return ()
        Just minTime -> do
            pid <- getSelfPid
            diff <- getCurrentTime >>= return . diffUTCTime minTime
            tickAt pid diff
  where tickAt pid diff
          | (diff <= 0) = do
              send pid Tick
              tickerRef .= Nothing
          | otherwise = do
            let interval = (diffTimeToTimeInterval diff)
            ref <- liftProcess $ sendAfter interval pid Tick
            tickerRef .= (Just ref)

doAddSchedule :: Event -> ScheduleAgentET ()
doAddSchedule event = do
    now <- getCurrentTime
    () <- update (AddEvent $ calcNextScheduled now event)
    lift scheduleNextTick

doRemoveSchedule :: Key -> ScheduleAgentET ()
doRemoveSchedule key' = do
    update (RemoveEvent key')

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
                         ++ (unpack format)

deriveSerializers ''ScheduleCommand
deriveSerializers ''RetryOption

instance Binary ScheduleFail
deriveNFData ''ScheduleFail

deriveSerializers ''ScheduleRecurrence
deriveSerializers ''Event
deriveSafeStore ''SchedulePersist
