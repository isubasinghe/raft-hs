{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import Control.Applicative
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Acid
import Data.Acid.Advanced
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable
import System.Environment
import System.IO

type Key = String

type Value = String

newtype KeyValue = KeyValue (Map.Map Key Value)
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value =
  do
    KeyValue m <- State.get
    State.put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key =
  do
    KeyValue m <- ask
    return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

asd :: IO ()
asd = do
  acid <- openLocalState (KeyValue Map.empty)

  closeAcidState acid
