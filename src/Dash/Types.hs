{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dash.Types where

import           BasicPrelude
import           Database.LevelDB.Higher hiding (get, put)
import qualified Prelude        as P
import           Data.Typeable  (mkTyConApp, mkTyCon3, TypeRep)
import           Data.SafeCopy
import           Data.Serialize

import           Dash.Store (Stashable(..), FetchFail(..), decodeStore)


data RunStatus = Running (Maybe String)
               | Complete (Maybe String)
               | Failed (Maybe String)
    deriving (Show, Eq)

class Runnable a where
    exec :: a -> IO RunStatus

-- | Wrapper lets us store an Action and recover it using
-- the type name in 'registerUnWrappers'
data Wrapper = Wrapper { wType :: ByteString
                       , value :: ByteString }
                deriving (Show, Typeable)

deriveSafeCopy 1 'base ''Wrapper

instance Serialize Wrapper where
    get = safeGet
    put = safePut

data Action a = forall p. (Stashable p, Runnable p, Typeable p) => Action p

instance Typeable (Action a) where
    typeOf _ = mkTyConApp (mkTyCon3 "dash" "Dash.Action" "Action") []

instance Eq (Action a) where
    a == b = encode a == encode b

instance P.Show (Action a) where
    show (Action a) = "Action (" ++ P.show a ++ ")"

instance Serialize (Action a) where
    put (Action a) = put a
    get = error "decode/get directly on Action can't happen; use decodeAction"

instance Stashable (Action a) where
    key (Action s) = key s

instance Runnable (Action a) where
    exec (Action s) = exec s
