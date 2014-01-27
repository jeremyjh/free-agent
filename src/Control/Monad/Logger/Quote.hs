{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Provides interpolated text QuasiQuotations for MonadLogger
-- based logging systems
module Control.Monad.Logger.Quote
    ( qinfo
    , qdebug
    , qwarn
    , qerror
    , qinfoNS
    , qdebugNS
    , qwarnNS
    , qerrorNS
    ) where

import           Data.Char                  (isSpace, isControl)
import qualified Data.Text                  as ST
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Builder     (fromText, toLazyText)
import           Language.Haskell.TH.Quote  (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (Lift (lift), Q
                                            , Exp(AppE), qLocation)

import           Control.Monad.Logger       (LogLevel(..), monadLoggerLog, liftLoc)
import           Text.Shakespeare           ( ShakespeareSettings(..)
                                            , defaultShakespeareSettings
                                            , shakespeareFromString)

settings :: Q ShakespeareSettings
settings = do
  toTExp <- [|fromText . ST.pack . show|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
      , wrap = wrapExp
      , unwrap = unWrapExp
  }

-- | Quoter for logging interpolated strings at LevelInfo
qinfo :: QuasiQuoter
qinfo = qlog True LevelInfo

-- | Quoter for logging interpolated strings at LevelDebug
qdebug :: QuasiQuoter
qdebug = qlog True LevelDebug

-- | Quoter for logging interpolated strings at LevelWarn
qwarn :: QuasiQuoter
qwarn = qlog True LevelWarn

-- | Quoter for logging interpolated strings at LevelError
qerror :: QuasiQuoter
qerror = qlog True LevelError

-- | Same as qinfo, but does not "squish" the whitespace
qinfoNS :: QuasiQuoter
qinfoNS = qlog False LevelInfo

-- | Same as qdebug, but does not "squish" the whitespace
qdebugNS :: QuasiQuoter
qdebugNS = qlog False LevelDebug

-- | Same as qwarn, but does not "squish" the whitespace
qwarnNS :: QuasiQuoter
qwarnNS = qlog False LevelWarn

-- | Same as qerror, but does not "squish" the whitespace
qerrorNS :: QuasiQuoter
qerrorNS = qlog False LevelError

qlog :: Bool -> LogLevel -> QuasiQuoter
qlog doSquish level =
    QuasiQuoter { quoteExp = \s -> do
        rs <- settings
        mlog <- [| monadLoggerLog $(qLocation >>= liftLoc) (ST.pack "") $(lift level) . (id :: ST.Text -> ST.Text) . LT.toStrict . toLazyText |]
        interpolated <- shakespeareFromString rs { justVarInterpolation = True } $ squishIf s
        return (mlog `AppE` interpolated)
    }
  where squishIf s' = if doSquish then squish s' else s'

-- replace consecutive spaces with a single space
-- replace tab, newline with a space
squish :: String -> String
squish = reverse . doSquish ""
  where
    doSquish acc [] = acc
    doSquish (' ':acc) (x:rest)
        | isSpace x = doSquish (' ':acc) rest
    doSquish acc (x:rest)
        | isControl x = doSquish (' ':acc) rest
        | otherwise = doSquish (x:acc) rest
