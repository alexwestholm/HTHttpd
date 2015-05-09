module HTHttpd.Parser (parseMessage, parseGetParams) where

import Control.Applicative ((<*>),(<$>))
import Data.Maybe (isJust)
import Network.URI (parseURIReference, uriPath, uriAuthority, uriIsRelative, uriRegName, uriPort)
import qualified Data.Map as M
import qualified Data.List.Split as SP

import HTHttpd.Types

-- The parsing functions below look awfully similar. Why not group them in a
-- typeclass? Because it doesn't gain us anything: we're still in the Stringly
-- typed area, meaning that all parsing functions dispatch on String.
-- That means we can't have a single parse function without somehow annotating
-- its call site to avoid problems with type inference. That should clue us
-- in to the fact that a typeclass gains us nothing here.

parseRequestType :: String -> Either String RequestMethod
parseRequestType "GET" = Right GetRequest
parseRequestType "POST" = Right PostRequest
parseRequestType "PUT" = Right PutRequest
parseRequestType "DELETE" = Right DeleteRequest
parseRequestType raw = Left $ "Unknown HTTP Request Method: " ++ raw

parseVersion :: String -> Either String HTTPVersion
parseVersion "HTTP/1.0" = Right HTTP1dot0
parseVersion "HTTP/1.1" = Right HTTP1dot1
parseVersion raw = Left $ "Unsupported HTTP Version: " ++ raw

parseUri :: String -> Either String URI
parseUri raw = case parseURIReference raw of
  Just parsed -> parseUri' parsed
  Nothing -> Left $ "Invalid URI: " ++ raw
  where parseUri' fields
          | uriIsRelative fields = Right $ AbsPath path
          | isJust authority && path == "" =
            let (Just a) = authority
            in Right $ Authority (uriRegName a) (uriPort a)
          | otherwise = Right $ AbsURI $ show fields
          where path = uriPath fields
                authority = uriAuthority fields

pairFold :: String -> String -> M.Map String String -> M.Map String String
pairFold separator keyAndVal result
  | length parts > 1 = M.insert k v result
  | otherwise = result -- leave accumulator unchanged if split fails
  where parts = SP.splitOn separator keyAndVal
        (k:v:_) = parts

parseHeaders :: [String] -> MessageHeaders
parseHeaders = MessageHeaders . foldr (pairFold ":") M.empty

parseGetParams :: String -> RouteParams
parseGetParams = RouteParams . foldr (pairFold "=") M.empty . SP.splitOn "&"

-- The following two functions become a boundary, past which we live in a
-- nice strongly typed world.
parseRequestLine :: String -> Either String RequestLine
parseRequestLine raw
  | length fields < 3 = Left $ "Malformed HTTP request: " ++ raw
  -- Applicative usage below is quite convenient... essentially we short
  -- circuit if anything fails to parse, returning the relevant Left value.
  | otherwise = RequestLine <$>
    parseRequestType methodRaw <*>
    parseUri uriRaw <*>
    parseVersion versionRaw
  where (methodRaw:uriRaw:versionRaw:_) = fields
        fields = words raw -- separated for the first guard.

-- NOTE: this really only handles GET requests properly at the moment.
-- fairly easy to modify to support others, but left as an exercise for
-- the reader. (hint: start with another field in RequestMessage for body).
parseMessage :: [String] -> Either String RequestMessage
parseMessage raw
  | length strippedRaw < 1 = Left "Invalid HTTP Message Format"
  | otherwise = parseRequestLine reqLine >>= \req ->
    Right $ RequestMessage req $ parseHeaders headerLines
  where strippedRaw = dropWhile (== "") raw -- per RFC 2616, strip leading space
        ([reqLine],headerLines) = splitAt 1 strippedRaw
        -- Another next step would be separating headerLines into actual
        -- header elements and request body for requests like POSTs that
        -- send data in a body.
