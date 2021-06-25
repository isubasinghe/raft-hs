{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}


module Raft where
import GHC.Generics
import NetworkManager
import Data.ByteString
import Codec.Serialise

newtype CommitEntry = CommitEntry (ByteString, Integer)
  deriving(Show, Generic)

instance Eq CommitEntry where
    (CommitEntry (_,v1)) == (CommitEntry (_,v2)) = v1 == v2

instance Ord CommitEntry where
    (CommitEntry (_, v1)) `compare` (CommitEntry (_, v2)) = v1 `compare` v2

instance Serialise CommitEntry

data NodeState = Leader | Follower

data RaftState = RaftState
  {  currentTerm :: Int
  ,  votedFor    :: Maybe Int
  ,  log         :: [CommitEntry]
  ,  commitIndex :: Int
  ,  lastApplied :: Int 
  ,  nextIndex   :: Maybe [Int]
  ,  matchIndex  :: Maybe [Int]
  }
  deriving(Show, Eq, Generic)


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

appendEntries :: RaftState -> (RaftState, AppendEntriesResponse)
appendEntries s = undefined