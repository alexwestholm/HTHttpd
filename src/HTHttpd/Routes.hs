module HTHttpd.Routes (
    getRoutes
   ,postRoutes
   ,putRoutes
   ,deleteRoutes
  ) where

import qualified Data.Map as M

import HTHttpd.Types
import HTHttpd.Handlers

getRoutes :: RouteMap
getRoutes = [
   (Route "/cool", \_ _ ->
      send200 Nothing "<html><body><h1>COOL!</h1></body></html>")
  ,(Route "/hello/:name", \p _ ->
      case M.lookup ":name" (fetchParams p) of
        Just hiGuy -> send200 Nothing ("<html><body>Hi, "++hiGuy++"!</body></html>")
        Nothing -> send404)
  ,(Route "*", \_ _ ->
      send200 Nothing "<html><body><p>Yep, default works</p></body></html>")
  ]

postRoutes :: RouteMap
postRoutes = []

deleteRoutes :: RouteMap
deleteRoutes = []

putRoutes :: RouteMap
putRoutes = []

