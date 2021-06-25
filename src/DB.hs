{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TemplateHaskell    #-}

module DB where

import           Data.Acid
import           Data.Acid.Advanced

import           Control.Applicative
import           Control.Monad.Reader
import qualified Control.Monad.State  as State
import           Data.SafeCopy
import           System.Environment
import           System.IO

import           Data.Typeable

import qualified Data.Map             as Map

type Key = String
type Value = String

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- State.get
         State.put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])
