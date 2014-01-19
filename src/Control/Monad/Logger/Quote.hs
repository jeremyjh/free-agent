{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Control.Monad.Logger.Quote
    ( qinfo
    , qdebug
    , qwarn
    , qerror
    ) where

import           Data.Char (isSpace, isControl)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (fromText, toLazyText)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax (Lift (lift), Q, Exp(AppE), qLocation)

import           Control.Monad.Logger
import           Text.Shakespeare

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
qinfo = qlog LevelInfo

-- | Quoter for logging interpolated strings at LevelDebug
qdebug :: QuasiQuoter
qdebug = qlog LevelDebug

-- | Quoter for logging interpolated strings at LevelWarn
qwarn :: QuasiQuoter
qwarn = qlog LevelWarn

-- | Quoter for logging interpolated strings at LevelError
qerror:: QuasiQuoter
qerror= qlog LevelError

qlog :: LogLevel -> QuasiQuoter
qlog level =
    QuasiQuoter { quoteExp = \s -> do
        rs <- settings
        mlog <- [| monadLoggerLog $(qLocation >>= liftLoc) (ST.pack "") $(lift LevelDebug) . (id :: ST.Text -> ST.Text) . LT.toStrict . toLazyText |]
        interpolated <- shakespeareFromString rs { justVarInterpolation = True } $ squish s
        return (mlog `AppE` interpolated)
    }

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
