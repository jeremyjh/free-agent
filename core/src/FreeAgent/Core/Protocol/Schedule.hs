{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards    #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes                                #-}


module FreeAgent.Core.Protocol.Schedule where

import           FreeAgent.AgentPrelude
import           FreeAgent.Core.Internal.Lenses
import           FreeAgent.Core.Protocol
import           FreeAgent.Orphans                          ()

import           Control.Error                              (note)
import           Control.Monad.State                        (StateT)


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

instance IsString ScheduleRecurrence where
    fromString format =
        case cronEvent (pack format) of
            Right cron' -> cron'
            _ -> error $ "Unable to parse cron formatted literal: "
                         ++ unpack format
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


type ScheduleImplM st rs = StateT st Agent rs
type ScheduleImplE st rs = ScheduleImplM st (Either ScheduleFail rs)

data ScheduleImpl st = ScheduleImpl {
    callScheduleAddEvent :: ScheduleAddEvent -> ProtoT ScheduleAddEvent st ()
  , callScheduleEventControl :: ScheduleEventControl -> ProtoT ScheduleEventControl st ()
  , callScheduleLookupEvent :: ScheduleLookupEvent -> ProtoT ScheduleLookupEvent st (Maybe Event)
  , callScheduleQueryEvents :: ScheduleQueryEvents -> ProtoT ScheduleQueryEvents st [Event]
  , callScheduleRemoveEvent :: ScheduleRemoveEvent -> ProtoT ScheduleRemoveEvent st (Either ScheduleFail ())
  , castScheduleControl :: ScheduleControl -> ProtoT ScheduleControl st ()
}

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

data ScheduleLookupEvent = ScheduleLookupEvent Key
    deriving (Show, Typeable, Generic)

instance NFData ScheduleLookupEvent where rnf = genericRnf
instance Binary ScheduleLookupEvent


instance ServerCall ScheduleLookupEvent where
    type CallProtocol ScheduleLookupEvent = ScheduleImpl
    type CallResponse ScheduleLookupEvent = Maybe Event
    callName _ = serverName
    respond = callScheduleLookupEvent

data ScheduleRemoveEvent = ScheduleRemoveEvent Key
    deriving (Show, Typeable, Generic)

instance Binary ScheduleRemoveEvent
instance NFData ScheduleRemoveEvent where rnf = genericRnf

instance ServerCall ScheduleRemoveEvent where
    type CallProtocol ScheduleRemoveEvent = ScheduleImpl
    type CallResponse ScheduleRemoveEvent = Either ScheduleFail ()
    callName _ = serverName
    respond = callScheduleRemoveEvent

serverName :: String
serverName = "agent:schedule"

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
