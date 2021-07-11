{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Raft where
import Prelude hiding (log, id)
import GHC.Generics
import Data.ByteString
import Codec.Serialise
import Control.Concurrent.MVar
import Control.Monad.Reader
import NetworkManager as NM

class ApplicationEncDec a where
  decode :: ByteString -> a
  encode :: a -> ByteString

class ApplicationEncDec a => RaftListener a where
  handle :: a -> IO ()

newtype CommitEntry = CommitEntry (ByteString, Integer)
  deriving(Show, Generic)

instance Eq CommitEntry where
    (CommitEntry (_,v1)) == (CommitEntry (_,v2)) = v1 == v2

instance Ord CommitEntry where
    (CommitEntry (_, v1)) `compare` (CommitEntry (_, v2)) = v1 `compare` v2

instance Serialise CommitEntry

data NodeState = Leader | Follower

data RaftState = RaftState
  {  id          :: Int  
  ,  currentTerm :: Int
  ,  votedFor    :: Maybe Int
  ,  log         :: [CommitEntry]
  ,  commitIndex :: Int
  ,  lastApplied :: Int 
  ,  nextIndex   :: Maybe [Int]
  ,  matchIndex  :: Maybe [Int]
  }
  deriving(Show, Eq, Generic)


newRaftState :: Int -> RaftState
newRaftState id = RaftState 
  { id = id
  , currentTerm = 0
  , votedFor = Nothing 
  , log = []
  , commitIndex = 0
  , lastApplied = 0
  , nextIndex = Nothing 
  , matchIndex = Nothing  
  }


data RequestMessage = AppendEntries 
  {  aterm        :: Int
  ,  leaderId     :: Int
  ,  prevLogIndex :: Int
  ,  entries      :: [CommitEntry]
  ,  leaderCommit :: Int
  }
  | RequestVote 
  {  rterm        :: Int
  ,  candidateId  :: Int
  ,  lastLogIndex :: Int 
  ,  lastLogTerm  :: Int 
  }
  deriving(Show, Eq, Generic)

instance Serialise RequestMessage


data AppendEntriesResponse = AppendEntriesResponse Int Bool
  deriving(Show, Eq, Generic)

instance Serialise AppendEntriesResponse

data RequestVoteResponse = RequestVoteResponse Int Bool
  deriving(Show, Eq, Generic)

instance Serialise RequestVoteResponse





-- runRaftService :: NM.Config -> IO ()
-- runRaftService config = 