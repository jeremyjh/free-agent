{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Dash(dashMain) where

import qualified Data.ByteString.Char8             as BS
import           Control.Monad.Trans.Resource      (release, ResIO, runResourceT)

import           Dash.Prelude
import           Dash.Proto
import           Dash.Store
import           Dash.Plugins.Nagios

import           Data.Default
import           Database.LevelDB hiding (get, put)

import           Control.Monad.Reader

dashMain :: IO ()
dashMain =  do
    runDB "/tmp/leveltest10" "hello" $ do
        putR "first" "firstvalue"
        putR "second" "secondvalue"

    {-(Just val) <- get db "hello#first"-}
    {-liftIO $ BS.putStrLn val-}
    {-put db def "hello" "something"-}

    {-(Right wrapper) <- fetch db "jeremyhuffman.com"-}
    {-let (Right cmd) = unWrap wrapper-}
    {-liftIO $ BS.putStrLn $ fromU $ host cmd-}
    return ()
  where
    printIt iter = do
        (Just v) <- iterValue iter
        liftIO $ BS.putStrLn v
