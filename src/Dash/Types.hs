{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dash.Types where

import           BasicPrelude
import qualified Prelude              as P
import           Control.Monad.Reader (ReaderT)
import           Data.Typeable        (mkTyConApp, mkTyCon3)
import           Data.SafeCopy
import           Data.Serialize

import           Dash.Store           (Stashable(..), FetchFail)


data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus


-- | Wrapper lets us store an Actionnd recover it using
-- the type name in 'registerUnWrappers'
data Wrapped = Wrapped { wType :: ByteString
                       , value :: ByteString }
                deriving (Show, Typeable)

deriveSafeCopy 1 'base ''Wrapped

instance Serialize Wrapped where
    get = safeGet
    put = safePut

data Action = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable Action where
    typeOf _ = mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

instance Eq Action where
    a == b = encode a == encode b

instance P.Show Action where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance Serialize Action where
    put (Action a) = put a
    get = error "decode/get directly on Action can't happen; use decodeAction"

instance Stashable Action where
    key (Action a) = key a

instance Runnable Action where
    exec (Action a) = exec a

instance SafeCopy Action where
    putCopy (Action a) = putCopy a

type FetchAction = Either FetchFail Action
type UnWrapper a = (Wrapped -> Either FetchFail a)
type PluginUnWrapper a = (ByteString, UnWrapper a)

type PluginMap = Map ByteString (UnWrapper Action)
data AgentConfig = AgentConfig { configPlugins :: PluginMap }
type Config m a = ReaderT AgentConfig m a
