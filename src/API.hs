{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Control.Concurrent.STM
import Control.Monad.Reader
import DB
import Data.Acid
import Data.Default.Class
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text.Lazy (Text, pack, unpack)
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Text.Read (readMaybe)
import Web.Scotty.Trans

newtype DBState = DBState {state :: AcidState KeyValue}

newtype WebM a = WebM {runWebM :: ReaderT DBState IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader DBState)

portEnvKey :: String
portEnvKey = "PORT"

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: String -> DBState -> WebM (Maybe String)
gets key db = do
  liftIO $ query acid (LookupKey key)
  where
    acid = state db

modify :: String -> String -> DBState -> WebM ()
modify key value db = do
  liftIO $ update acid (InsertKey key value)
  where
    acid = state db

runApp :: IO ()
runApp = do
  acid <- openLocalState $ KeyValue Map.empty
  portString <- lookupEnv "PORT"
  let port = fromMaybe 3000 $ maybe (Just 3000) (readMaybe :: String -> Maybe Int) portString
  let runActionToIO m = runReaderT (runWebM m) $ DBState acid
  scottyT port runActionToIO app

app :: ScottyT Text WebM ()
app = do
  middleware logStdout
  get "/insert/:key/:value" $ do
    db <- ask
    key <- param "key"
    value <- param "value"
    webM $ modify key value db
    redirect $ pack $ "/read/" ++ key
  get "/read/:key" $ do
    db <- ask
    key <- param "key"
    out <- webM $ gets key db
    case out of
      Nothing -> text "<Key not present>"
      Just a -> text $ pack $ concatMap f a
  where
    f s = case s of
      '<' -> "\\<"
      '>' -> "\\>"
      q -> [q]

key :: Text
key = error "not implemented"
