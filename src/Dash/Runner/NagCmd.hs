module Dash.Runner.NagCmd(exec) where

import System.Process(readProcess)

exec :: IO String
exec = readProcess "./thing.sh" [] []
