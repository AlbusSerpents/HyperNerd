{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot
import BotState
import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Database.SQLite.Simple as SQLite
import IrcTransport
import qualified Sqlite.EntityPersistence as SEP
import System.Clock
import System.Environment
import Irc.Commands
import qualified Data.Text as T
import System.IO

eventLoop :: Bot -> TimeSpec -> BotState -> IO ()
eventLoop b prevCPUTime botState = do
  threadDelay 10000 -- to prevent busy looping
  currCPUTime <- getTime Monotonic
  let deltaTime = toNanoSecs (currCPUTime - prevCPUTime) `div` 1000000
  pollMessage <-
    maybe return (handleIrcMessage b) <$>
    atomically (tryReadTQueue $ bsIncoming botState)
  pollMessage botState >>= advanceTimeouts deltaTime >>= eventLoop b currCPUTime

-- TODO: can we use withBotState in logicEntry?
logicEntry :: IncomingQueue -> OutcomingQueue -> Config -> String -> IO ()
logicEntry incoming outcoming conf databasePath =
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    currCPUTime <- getTime Monotonic
    let botState =
          BotState
            { bsConfig = conf
            , bsSqliteConn = sqliteConn
            , bsTimeouts = []
            , bsIncoming = incoming
            , bsOutcoming = outcoming
            }
    joinChannel bot botState >>= eventLoop bot currCPUTime

mainWithArgs :: [String] -> IO ()
mainWithArgs [configPath, databasePath] = do
  incoming <- atomically newTQueue
  outcoming <- atomically newTQueue
  conf <- configFromFile configPath
  void $ forkIO $ ircTransportEntry incoming outcoming configPath
  logicEntry incoming outcoming conf databasePath
mainWithArgs _ = error "./HyperNerd <config-file> <database-file>"

main :: IO ()
main = getArgs >>= debugMain

withBotState :: String -> Config -> (BotState -> IO ()) -> IO ()
withBotState databasePath conf block = do
  incoming <- atomically newTQueue
  outcoming <- atomically newTQueue
  SQLite.withConnection databasePath $ \sqliteConn -> do
    SEP.prepareSchema sqliteConn
    let botState =
          BotState
            { bsConfig = conf
            , bsSqliteConn = sqliteConn
            , bsTimeouts = []
            , bsIncoming = incoming
            , bsOutcoming = outcoming
            }
    block botState

debugMain :: [String] -> IO ()
debugMain [configPath, databasePath] = do
  conf <- configFromFile configPath
  withBotState databasePath conf $ \botState -> do
    joinChannel bot botState >>= debugLoop bot
debugMain _ = error "Config and database paths are not provided"

-- TODO: Name `dumpOutcomingQueue` doesn't reflect the purpose of the function
dumpOutcomingQueue :: OutcomingQueue -> IO ()
dumpOutcomingQueue q = do
  mb <- atomically (tryReadTQueue q)
  case mb of
    Just m -> do
      putStr "Bot says: "
      print m
      dumpOutcomingQueue q
    Nothing -> return ()

debugLoop :: Bot -> BotState -> IO ()
debugLoop b state = do
  dumpOutcomingQueue $ bsOutcoming state
  putStr "> "
  hFlush stdout
  text <- getLine
  -- TODO: message produced by ircPrivmsg cannot be handled by handleIrcMessage
  handleIrcMessage b (ircPrivmsg "#debug" $ T.pack text) state >>= debugLoop b
