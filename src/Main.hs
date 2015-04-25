{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.Text (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Network.URL (decString)

import Happstack.Server
import Network.WebSockets

main :: IO ()
main = do

  tchan <- newTChanIO
  tusrs <- newTVarIO Set.empty

  putStrLn "Starting servers..."

  -- start websockets in background
  forkIO $ runServer "0.0.0.0" 8099 $ wsApp tusrs tchan

  -- run HTTP server in main thread
  let conf = nullConf { port = 8098 }
  simpleHTTP conf $ serveDirectory DisableBrowsing ["index.html"] "static"


-- | Main WebSockets application
wsApp :: TVar Users -> TChan ChatMessage -> ServerApp
wsApp tusrs tmsgs pending_con = do

  -- check if nickname is available
  ok <- atomically $ do
    usrs <- readTVar tusrs
    let ok = not $ Set.member nick usrs
    when ok $ modifyTVar tusrs $ Set.insert nick
    return ok

  -- more nickname validation
  let tooshort = Text.length nick < 3
      toolong  = Text.length nick > 20
      valid    = Text.all isAlphaNum nick

  if not ok || toolong || tooshort || not valid then
    rejectRequest pending_con "Nickname not available."
   else do
    con <- acceptRequest pending_con

    -- start ping thread
    forkPingThread con $ 5 * 60 -- 5 minutes

    -- "subscribe" to incoming messages from other users
    tinc <- atomically $ cloneTChan tmsgs
    tid <- forkIO $ ignoreClosed $ forever $ do
      cm <- atomically $ readTChan tinc
      sendTextData con cm

    -- receive messages
    removeNickWhenClosed tid $ forever $ do
      msg <- receiveData con
      unless (Text.all isSpace msg) $
        atomically $ writeTChan tinc $ ChatMessage nick msg

 where

  -- request path = nickname to use
  nick = textlift (decString False) . Text.drop 1 . Text.decodeUtf8
       . requestPath $ pendingRequest pending_con
   where
    textlift f t = maybe t Text.pack . f $ Text.unpack t

  ignoreClosed = handle $ \(_ :: SomeException) -> return ()

  removeNickWhenClosed tid = handle $ \ce -> do
    case ce of
      ParseException _ -> return () -- ignore
      _ -> do
        -- remove nickname from user list
        atomically $ modifyTVar tusrs $ Set.delete nick
        killThread tid


--------------------------------------------------------------------------------
-- | The main message type
--
data ChatMessage = ChatMessage
  { msgFrom    :: Text
  , msgMessage :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON ChatMessage where
  parseJSON (Object cm) = ChatMessage <$> cm .: "f" <*> cm .: "m"
  parseJSON _           = mzero

instance ToJSON ChatMessage where
  toJSON (ChatMessage f m) = object [ "f" .= f, "m" .= m ]

instance WebSocketsData ChatMessage where
  fromLazyByteString bs
    | Just json <- decode bs
    , Success a <- fromJSON json
    = a
    | otherwise
    = error "No parse for WebSockets data"
  toLazyByteString = encode . toJSON

type Users = Set Text
