module HTHttpd.Router (dispatchRequest) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import qualified Data.Map as M
import qualified Data.List.Split as SP

import HTHttpd.Types
import HTHttpd.Parser
import HTHttpd.Routes
import HTHttpd.Handlers

matchRoute :: URI -> Route -> Maybe RouteParams
matchRoute path (Route route')
  -- default route always matches
  | route' == "*" = Just $ RouteParams (M.fromList [])
  -- route doesn't match if different number of path segments
  | length pathSects /= length routeSects = Nothing 
  -- match against the route and collect route variables
  | otherwise = case matchComponents pathSects routeSects $ M.fromList [] of
    Just routeParams -> Just . RouteParams $ M.union (fetchParams getParams) routeParams
    Nothing -> Nothing
  where urlParts = SP.splitOn "?" $ show path
        pathSects = SP.splitOn "/" (head urlParts)
        routeSects = SP.splitOn "/" route'
        getParams = if length urlParts > 1
                    then parseGetParams (urlParts !! 1)
                    else RouteParams M.empty

matchComponents :: [String] -> [String] -> M.Map String String -> Maybe (M.Map String String)
matchComponents [] [] m = Just m -- return accumulator only if we've matched every component
matchComponents [] _ _ = Nothing -- if we finish the path but there's more route, abort.
matchComponents _ [] _ = Nothing -- if we finish the route but there's more path, abort.
matchComponents (p:ps) (r:rs) m
  -- if segments don't match and this segment isn't variable in the route, abort.
  | p /= r && not (":" `isInfixOf` r) = Nothing
  -- if they don't match but it is a variable segment, capture the result
  | p /= r = matchComponents ps rs $ M.insert r p m
  -- if the segments match, continue processing w/o capturing any values
  | otherwise = matchComponents ps rs m

-- Iterate over RouteMap keys looking for matches, holding onto RouteParams
-- if match occurs then dispatching to the key in the RouteMap
dispatchRouteMap :: RequestMessage -> RouteMap -> ResponseGenerator
dispatchRouteMap _ [] = send404
dispatchRouteMap req ((rt, handler):rts) =
  case matchRoute (uri $ target req) rt of 
    Nothing -> dispatchRouteMap req rts
    Just params -> handler params (headers req)

readRequestFile :: FilePath -> RequestMessage -> IO (Maybe String)
readRequestFile dr r = do
  let path = pathPart (show . uri $ target r)
      filePath = dr ++ path
  filePresent <- doesFileExist filePath
  if filePresent
    then readFile filePath >>= \body -> return $ Just body
    else return Nothing
  where pathPart = dropWhile (/= '/')
  
-- GET Requests have some IO due to needing to read file contents when
-- serving assets. The above function relates to this as well.
dispatchGetRequest :: RequestMessage -> RouteMap -> ResponseGenerator
dispatchGetRequest req rts hndl = do
  dr <- asks docRoot
  contents <- liftIO $ readRequestFile dr req
  let generator = case contents of
                    Just fsData -> send200 Nothing fsData
                    Nothing -> dispatchRouteMap req rts
  generator hndl

dispatchRequest :: RequestMessage -> ResponseGenerator
dispatchRequest req =
  case method $ target req of
    GetRequest -> dispatchGetRequest req getRoutes
    PostRequest -> dispatchRouteMap req postRoutes
    PutRequest -> dispatchRouteMap req putRoutes
    DeleteRequest -> dispatchRouteMap req deleteRoutes

-- Compared to the other version, no needless passing/returning of IO related types here.
-- Our type synonym wraps up all of that, allowing us to maintain purity.
