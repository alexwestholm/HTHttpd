module Main where

import System.IO (Handle, hGetLine, hSetBuffering, hClose, BufferMode (LineBuffering))
import System.Directory (getHomeDirectory)
import Control.Monad.Reader (runReaderT)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Time (getCurrentTime)
import Network (withSocketsDo, PortID (PortNumber), accept, listenOn)
import Prelude hiding (getContents)

import HTHttpd.Types
import HTHttpd.Router
import HTHttpd.Parser
import HTHttpd.Handlers

readMessage :: Handle -> IO [String]
readMessage hndl = readMessage' hndl []
  where blankInput acc | length acc > 0 = return acc
                       | otherwise = readMessage' hndl acc
        readMessage' h l = do
          raw <- hGetLine h
          let nput = takeWhile (/='\r') raw
          if nput == ""
            then blankInput l
            else readMessage' h $ l ++ [nput]

-- This function is the main body of the request/response cycle
processRequest :: Handle -> FilePath -> IO ()
processRequest hndl home = do
  raw <- readMessage hndl
  rightNow <- getCurrentTime
  let environment = RequestEnv rightNow (home ++ "/web")
      fullReq = dropWhile (== "") raw
  -- putStr $ head fullReq ++ " -> "-- NOOOO! UNSAFE! UNSAFE! UNSAFE!
  runReaderT (getGenerator fullReq hndl) environment
  where getGenerator req = case parseMessage req of
                             Right msg -> dispatchRequest msg
                             Left _ -> send400

-- No daemonizing, so we can keep that decoupled and use tooling like angel
main :: IO ()
main = withSocketsDo $ do -- for cross platform compatibility
  homeDir <- getHomeDirectory -- grab home as soon as we start
  sckt <- listenOn $ PortNumber 31337
  forever $ do
    (hndl,_,_) <- accept sckt
    forkIO $ do
      hSetBuffering hndl LineBuffering
      finally (processRequest hndl homeDir)
              (hClose hndl)

-- Major refactorings throughout this version:
-- 1) No more String usage.
-- 2) Enforcement of a type boundary
-- 3) Currying as a design feature (less volatile parameters go up front)
-- 4) Functional purity improved... keep IO isolated and to a minimum
-- 5) General DRYing up (note the ways that Haskell encourages DRYness)
-- 6) Error handling through Either, rather than exceptions and Maybe
-- 7) Usage of Control.Applicative, particularly for smart constructors
-- 8) Monadic threading (Reader) to eliminate passing variables around.
