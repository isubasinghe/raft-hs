{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API where

import Control.Concurrent.STM
import Control.Monad.Reader

import Data.String 
import Data.Text.Lazy (Text)
import Data.Default.Class
import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger

newtype AppState = AppState { tickcCount :: Int }


instance Default AppState where
  def = AppState 0


newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving(Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))


webM :: MonadTrans t => WebM a -> t WebM a
webM = lift


gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f



