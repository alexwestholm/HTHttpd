module HTHttpd.Handlers (
    sendResponse
   ,send200
   ,send400
   ,send404
   ,send500
  ) where

import Data.Monoid ((<>))
import Data.Time (UTCTime)
import System.IO (hFlush, hPutStrLn)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.UTF8 as BS

import HTHttpd.Types

send404 :: ResponseGenerator
send404 = sendResponse HTTP1dot1 HTTP404 Nothing errBody
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>404 Not Found</title></head>" ++
                  "<body><h1>Not Found</h1>" ++
                  "<p>But we didn't look too hard...</p>" ++
                  "</body></html>"

send500 :: ResponseGenerator
send500 = sendResponse HTTP1dot1 HTTP500 Nothing errBody
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>500 Internal Server Error</title></head>" ++
                  "<body><h1>Internal Server Error</h1>" ++
                  "<p>Our dog ate our homework...</p>" ++
                  "</body></html>"

send400 :: ResponseGenerator
send400 = sendResponse HTTP1dot1 HTTP400 Nothing errBody
  where errBody = "<!DOCTYPE html>" ++
                  "<meta charset=\"UTF-8\">" ++
                  "<head><title>400 Bad Request</title></head>" ++
                  "<body><h1>Bad Request</h1>" ++
                  "<p>What exactly are you trying to do, friend?</p>" ++
                  "</body></html>"

-- Through proper placement of less volatile params closer to the front of the arg list
-- this can now be written succinctly through partial application.
send200 :: Maybe MessageHeaders -> ResponseBody -> ResponseGenerator
send200 = sendResponse HTTP1dot1 HTTP200

standardHeaders :: MessageHeaders
standardHeaders = MessageHeaders $ M.fromList [
   ("Server", "HTHttpd/0.0.1 (linux)")
  ,("Connection","close")
  ,("Content-Type","text/html; charset=UTF-8")
  ]

responseHeaders :: UTCTime -> ResponseBody -> Maybe MessageHeaders -> MessageHeaders
responseHeaders when body = responseHeaders'
  -- We lift the real function body into the where clause to allow for sharing scope
  -- between the two primary equations.
  -- Note also that treating message headers as monoids allows us to change the
  -- underlying structure of the type later on, so long as it stays monoidal.
  where responseHeaders' Nothing = standardHeaders <> freshHeaders
        responseHeaders' (Just nHdrs) = standardHeaders <> nHdrs <> freshHeaders
        bsBody = BS.fromString body
        freshHeaders = MessageHeaders $ M.fromList [
          ("Content-Length", show $ BS.length bsBody),
          ("Date", show when)]

-- This function is the core of response IO.
sendResponse :: HTTPVersion -> HTTPStatus -> Maybe MessageHeaders -> ResponseBody -> ResponseGenerator
sendResponse v s h rb hndl = 
  asks responseAt >>= \sentAt ->
    liftIO $ do
      let status = show v ++ " " ++ show s
          updatedHeaders = show $ responseHeaders sentAt rb h
          response = status ++ "\n" ++ updatedHeaders ++ "\n" ++ rb
      putStrLn $ status ++ " (" ++ show sentAt ++ ")"
      hPutStrLn hndl response
      hFlush hndl
