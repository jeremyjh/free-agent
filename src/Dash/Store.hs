{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Dash.Store
    ( module Database.LevelDB.Higher
    , module Database.LevelDB.Higher.Store
    , Stashable(..)
    , stash,  stashB)
     where

import           Control.Monad.Writer (WriterT(..))
import           Database.LevelDB.Higher
import           Database.LevelDB.Higher.Store

-- | Types that can be serialized, stored and retrieved
--
class (Storeable a) => Stashable a where
    key :: a -> Key

-- | Save a serializable type with an instance for Stash
-- which provides the key.
--
stash :: (MonadLevelDB m, Stashable s)
      => s -> m ()
stash s = store (key s) s

-- | Store the 'Stashable' in the database - batch mode with 'runBatch'
--
stashB :: (MonadLevelDB m, Stashable s)
       => s -> WriterT WriteBatch m ()
stashB s = storeB (key s) s
