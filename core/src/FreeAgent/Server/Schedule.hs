{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards    #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes                                #-}


module FreeAgent.Server.Schedule where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Database.AcidState
import           FreeAgent.Orphans                          ()
import           FreeAgent.Process
import           FreeAgent.Core.Protocol.Executive   (ExecuteBatch(..))
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

import           Data.Aeson                                 (Result (..),
                                                             Value (..),
                                                             fromJSON, (.:?))
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH                              (Options (..),
                                                             defaultOptions,
                                                             deriveJSON)
import           Data.Attoparsec.Text                       (parseOnly)
import           Data.Binary                                (Binary)
import           Data.Char                                  as Char (toLower)
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
          , schedDisabled :: !Bool
          } deriving (Show, Eq, Typeable, Generic)

data ScheduleRecurrence
  = RecurCron !CronSchedule !Text -- ^ execute when schedule matches
  | RecurInterval !Int           -- ^ execute every n milliseconds
  | OnceAt !UTCTime
  deriving (Show, Eq, Typeable, Generic)

cronEvent :: Text -> Either String ScheduleRecurrence
cronEvent format =
    case parseOnly cronSchedule format of
        Right sched -> Right $ RecurCron sched format
        Left msg -> Left msg

instance ToJSON ScheduleRecurrence where
    toJSON (RecurCron _ expr) =
        A.object ["recurCron" A..= expr]
    toJSON (RecurInterval num) =
        A.object ["recurInterval" A..= num]
    toJSON (OnceAt time') =
        A.object ["onceAt" A..= time']

instance FromJSON ScheduleRecurrence where
    parseJSON (Object value') =
     do cron <- value' .:? "recurCron"
        interval <- value' .:? "recurInterval"
        once <- value' .:? "onceAt"
        case cron of
            Just expr ->
                case cronEvent expr of
                    Right recur -> return recur
                    Left _ -> mzero
            Nothing ->
                case interval of
                    Just i -> return (RecurInterval i)
                    Nothing ->
                        case once of
                            Just jtime ->
                                case fromJSON jtime of
                                    Error _ -> mzero
                                    Success time' -> return (OnceAt time')
                            Nothing -> mzero
    parseJSON _ = mzero

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
                   , stateTicking  :: Bool
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

scheduleRemoveEvent :: Key -> Update SchedulePersist ()
scheduleRemoveEvent key' = do
    events %= Map.delete key'
    nextScheduled %= Set.filter (\ (_, ev) -> key ev /= key')

disableEvents :: [Key] -> Update SchedulePersist ()
disableEvents keys =
 do events %= flip ( foldr $ \k es ->
                        Map.update (\e -> Just e {schedDisabled = True}) k es
                   ) keys
    nextScheduled %= Set.filter (\e -> schedKey (snd e) `notElem` keys)

enableEvents :: UTCTime -> [Key] -> Update SchedulePersist ()
enableEvents now keys =
 do events' <- use events
    forM_ keys $ \key' ->
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
$(makeAcidic ''SchedulePersist ['getPersist, 'addEvent, 'scheduleLookupEvent
                               , 'allCronEvents, 'queryAllEvents, 'readyToRun, 'nextScheduledTime
                               , 'scheduleRemoveEvent, 'disableEvents, 'enableEvents ])

-- ---------API---------------
-- API
-- ---------------------------

-- | Helper for 'ScheduleAddEvent'; schedules an Event. Note
-- that for a CronEvent, the event will run at the next matching time
-- beginning one minute from now - e.g. you are scheduling something to run
-- in the future, not right now.
schedule :: MonadAgent agent
         => Key -> ScheduleRecurrence -> RetryOption
         -> agent (Either CallFail ())
schedule key' recur retry =
    callServ (ScheduleAddEvent key' recur retry)

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


type ScheduleImplM st rs = StateT st Agent rs
type ScheduleImplE st rs = ScheduleImplM st (Either ScheduleFail rs)

data ScheduleImpl st = ScheduleImpl {
    callScheduleAddEvent :: ScheduleAddEvent -> ScheduleImplM st ()
  , callScheduleEventControl :: ScheduleEventControl -> ScheduleImplM st ()
  , callScheduleLookupEvent :: ScheduleLookupEvent -> ScheduleImplM st (Maybe Event)
  , callScheduleQueryEvents :: ScheduleQueryEvents -> ScheduleImplM st [Event]
  , callScheduleRemoveEvent :: ScheduleRemoveEvent -> ScheduleImplE st ()
  , castScheduleControl :: ScheduleControl -> ScheduleImplM st ()
}
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
    callScheduleEventControl' (ScheduleDisableEvents keys) =
        update (DisableEvents keys)

    callScheduleEventControl' (ScheduleEnableEvents keys) =
     do now <- getCurrentTime
        void $ update (EnableEvents now keys)
        scheduleNextTick
    callScheduleLookupEvent' = query
    callScheduleQueryEvents' _ = query QueryAllEvents
    callScheduleRemoveEvent' cmd = runLogEitherT cmd $ update cmd
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
                          liftProcess $ cancelTimer ref
                      Nothing -> return ()
          else [qinfo|Scheduler already stopped.|]

data ScheduleControl = ScheduleStart | ScheduleStop
    deriving (Show, Typeable, Generic)

instance Binary ScheduleControl
instance NFData ScheduleControl where rnf = genericRnf

instance ServerCast ScheduleControl where
    type CastProtocol ScheduleControl = ScheduleImpl
    castName _ = serverName
    handle = castScheduleControl

data ScheduleAddEvent
    = ScheduleAddEvent !Key !ScheduleRecurrence !RetryOption
    | ScheduleAddEvents [Event]
    | ScheduleAddNewerEvent !Key !ScheduleRecurrence !RetryOption !UTCTime
    deriving (Show, Typeable, Generic)

instance Binary ScheduleAddEvent
instance NFData ScheduleAddEvent where rnf = genericRnf

instance ServerCall ScheduleAddEvent where
    type CallProtocol ScheduleAddEvent = ScheduleImpl
    type CallResponse ScheduleAddEvent = ()
    callName _ = serverName
    respond = callScheduleAddEvent

data ScheduleEventControl
    = ScheduleDisableEvents ![Key]
    | ScheduleEnableEvents ![Key]
    deriving (Show, Typeable, Generic)

instance Binary ScheduleEventControl
instance NFData ScheduleEventControl where rnf = genericRnf

instance ServerCall ScheduleEventControl where
    type CallProtocol ScheduleEventControl = ScheduleImpl
    type CallResponse ScheduleEventControl = ()
    callName _ = serverName
    respond = callScheduleEventControl

data ScheduleQueryEvents = ScheduleQueryEvents
    deriving (Show, Typeable, Generic)

instance Binary ScheduleQueryEvents
instance NFData ScheduleQueryEvents where rnf = genericRnf

instance ServerCall ScheduleQueryEvents where
    type CallProtocol ScheduleQueryEvents = ScheduleImpl
    type CallResponse ScheduleQueryEvents = [Event]
    callName _ = serverName
    respond = callScheduleQueryEvents

deriving instance Generic ScheduleLookupEvent
deriving instance Show ScheduleLookupEvent
instance NFData ScheduleLookupEvent where rnf = genericRnf
instance Binary ScheduleLookupEvent

instance ServerCall ScheduleLookupEvent where
    type CallProtocol ScheduleLookupEvent = ScheduleImpl
    type CallResponse ScheduleLookupEvent = Maybe Event
    callName _ = serverName
    respond = callScheduleLookupEvent

deriving instance Generic ScheduleRemoveEvent
deriving instance Show ScheduleRemoveEvent
instance Binary ScheduleRemoveEvent
instance NFData ScheduleRemoveEvent where rnf = genericRnf

instance ServerCall ScheduleRemoveEvent where
    type CallProtocol ScheduleRemoveEvent = ScheduleImpl
    type CallResponse ScheduleRemoveEvent = Either ScheduleFail ()
    callName _ = serverName
    respond = callScheduleRemoveEvent

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
                \ s _ ->
             do case s ^. serverState.tickerRef of
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


instance IsString ScheduleRecurrence where
    fromString format =
        case cronEvent (pack format) of
            Right cron' -> cron'
            _ -> error $ "Unable to parse cron formatted literal: "
                         ++ unpack format


deriveSerializers ''RetryOption

instance Binary ScheduleFail
instance NFData ScheduleFail where rnf = genericRnf

instance Binary ScheduleRecurrence
instance NFData ScheduleRecurrence where rnf = genericRnf
deriveSafeStore ''ScheduleRecurrence

instance Binary Event
instance NFData Event where rnf = genericRnf
-- we want to customize the JSON field names for ShellCommand
-- so it looks nicer in Yaml, which may be very frequently used
deriveJSON (defaultOptions {fieldLabelModifier = \field ->
                                let (x:xs) = drop 5 field
                                in Char.toLower x : xs
                           })
           ''Event
deriveSafeStore ''Event

deriveSafeStore ''SchedulePersist
