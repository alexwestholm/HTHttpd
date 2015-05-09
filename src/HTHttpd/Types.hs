module HTHttpd.Types where
-- This module needs an explicit export list, and should hide value
-- constructors in favor of using smart constructors that parse and
-- validate at the type boundary.

import Data.Monoid (Monoid, mempty, mappend)
import Data.Time (UTCTime)
import System.IO (Handle)
import Control.Monad.Reader (ReaderT)
import qualified Data.Map as M

-- Our second approach encodes business logic within the types...
-- Note: algebraic data types look a lot like BNF/ABNF, which makes RFCs a great
-- resource for grokking ADT modeling.
-- This also makes Haskell superb for embedded DSLs!
data RequestMethod = GetRequest
                   | PostRequest
                   | PutRequest
                   | DeleteRequest
                   deriving (Eq,Show)

-- We don't suppose non-resource requests like OPTIONS, so no "*" URI
data URI = AbsPath String
         | Authority String String
         | AbsURI String
         deriving Eq

instance Show URI where
  show (AbsPath x) = x
  show (Authority x y) = x++":"++y
  show (AbsURI x) = x

data HTTPVersion = HTTP1dot0 | HTTP1dot1 deriving Eq
instance Show HTTPVersion where
  show HTTP1dot1 = "HTTP/1.1"
  show HTTP1dot0 = "HTTP/1.0"

-- Obviously, we're simplifying a bit here. Not to mention Network.HTTP.Types
-- ...but that would be less instructive.
data HTTPStatus = HTTP200 | HTTP400 | HTTP404 | HTTP500 deriving Eq
instance Show HTTPStatus where
  show HTTP200 = "200 OK"
  show HTTP400 = "400 Bad Request"
  show HTTP404 = "404 Not Found"
  show HTTP500 = "500 Internal Server Error"

-- No longer possible to construct an HTTPRequest that doesn't make sense
-- ...all components are required, and all must be well-formed.
data RequestLine = RequestLine {
   method :: RequestMethod
  ,uri :: URI
  ,version :: HTTPVersion
} deriving (Eq, Show)

-- Our static environment wrapper. This allows us to use the Reader monad
-- to cleanly thread values originating in the top level IO blocks
-- as deeply as we'd like in pure code. This helps keep code clean.
data RequestEnv = RequestEnv {
   responseAt :: UTCTime
  ,docRoot :: FilePath
}

-- Newtypes give us type safety, without runtime overhead and the ability to
-- make unique typeclass instances. Note: more sophisticated typing could
-- eventually be useful here, but since we basically ignore headers other
-- than making the map available, this is sufficient for now.
newtype MessageHeaders = MessageHeaders {
  getHeaders :: M.Map String String
} deriving Eq
  
instance Monoid MessageHeaders where
  mempty = MessageHeaders $ M.fromList []
  a `mappend` b = MessageHeaders (getHeaders a `mappend` getHeaders b)

instance Show MessageHeaders where
  show hdrs = M.foldWithKey (\k v a -> a ++ k ++ ": " ++ v ++ "\n") "" $ getHeaders hdrs

newtype RouteParams = RouteParams { fetchParams :: M.Map String String }
newtype Route = Route { fetchRoute :: String } deriving (Eq, Show)
-- Let's have a more sensible return type and threading for the route handlers, so
-- we can keep the routing code pure and isolate IO to the request reading and
-- response writing code.
type ResponseGenerator = Handle -> ReaderT RequestEnv IO ()
type RouteHandler = RouteParams -> MessageHeaders -> ResponseGenerator
type RouteMap = [(Route, RouteHandler)]
-- Type synonyms are OK here because we're primarily just using the above to save
-- ourselves typing. We're still type safe because of the component types.
-- Type synonyms also useful for clarifying arg list components:
type ResponseBody = String

data RequestMessage = RequestMessage {
   target :: RequestLine
  ,headers :: MessageHeaders
} deriving (Eq, Show)
