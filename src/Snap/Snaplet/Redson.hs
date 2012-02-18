{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Backbone.sync handler snaplet with Redis storage.

-}

module Snap.Snaplet.Redson (Redson
                           , redboneInit)
where

import Prelude hiding (concat)

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State

import Data.Aeson as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BZ (ByteString)
import qualified Data.ByteString.UTF8 as BU (fromString, toString)
import qualified Data.ByteString.Lazy.UTF8 as BZU (fromString)

import Data.Lens.Common
import Data.Lens.Template

import qualified Data.Map as M

import Data.Maybe

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.RedisDB
import Snap.Util.FileServe
import Text.Templating.Heist

import Network.WebSockets
import Network.WebSockets.Snap
import qualified Network.WebSockets.Util.PubSub as PS

import Database.Redis

import Snap.Snaplet.Redson.Util


data Event = Create | Update | Delete

------------------------------------------------------------------------------
-- | Event which can occur to model instance.
data ModelEvent = ModelEvent String String Event

------------------------------------------------------------------------------
-- | Redson snaplet state type.
data Redson = Redson
             { _database :: Snaplet RedisDB
             , _events :: PS.PubSub Hybi10
             }

makeLens ''Redson


------------------------------------------------------------------------------
-- | Render empty form for model.
emptyForm :: HasHeist b => Handler b Redson ()
emptyForm = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Serve JSON metamodel.
metamodel :: Handler b Redson ()
metamodel = ifTop $ do
  modelName <- liftM BU.toString getModelName
  serveFile $ "resources/static/js/models/" ++ modelName ++ ".js"


------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
getModelName:: MonadSnap m => m B.ByteString
getModelName = fromParam "model"


------------------------------------------------------------------------------
-- | Extract model instance id from request parameter.
getModelId:: MonadSnap m => m B.ByteString
getModelId= fromParam "id"


------------------------------------------------------------------------------
-- | Build Redis key given model name and id
modelKey :: B.ByteString -> B.ByteString -> B.ByteString
modelKey model id = B.concat [model, ":", id]


------------------------------------------------------------------------------
-- | Extract model instance Redis key from request parameters.
getModelKey :: MonadSnap m => m B.ByteString
getModelKey = liftM2 modelKey getModelName getModelId

------------------------------------------------------------------------------
-- | Get Redis key which stores id counter for model
modelIdKey :: B.ByteString -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]


------------------------------------------------------------------------------
-- | Get Redis key which stores timeline for model
modelTimeline :: B.ByteString -> B.ByteString
modelTimeline model = B.concat ["global:", model, ":timeline"]


------------------------------------------------------------------------------
-- | Builder for WebSockets message containing JSON describing creation or
-- deletion of model instance.
--
modelMessage :: B.ByteString 
             -> (B.ByteString 
                 -> B.ByteString 
                 -> Network.WebSockets.Message p)
modelMessage event = \model id ->
    let
        response :: [(B.ByteString, B.ByteString)]
        response = [("event", event),
                    ("id", id), 
                    ("model", model)]
    in
      DataMessage $ Text $ A.encode $ M.fromList response


creationMessage = modelMessage "create"
deletionMessage = modelMessage "delete"


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to B.ByteString with JSON.
--
-- @internal Note using explicit B.ByteString type over BS s as
-- suggested by redis because BS s doesn't imply ToJSON s
hgetallToJson :: [(B.ByteString, B.ByteString)] -> BZ.ByteString
hgetallToJson r = A.encode $ M.fromList r


------------------------------------------------------------------------------
-- | Decode B.ByteString with JSON to list of hash keys & values for
-- Redis HMSET
--
-- @return Nothing if parsing failed
jsonToHsetall :: BZ.ByteString -> Maybe [(B.ByteString, B.ByteString)]
jsonToHsetall s =
    let
        j = A.decode s
    in
      case j of
        Nothing -> Nothing
        Just m -> 
             -- Omit fields with null values and "id" key
            Just (map (\(k, v) -> (k, fromJust v)) $
                  filter (\(k, v) -> (isJust v && k /= "id")) $
                  M.toList m)


------------------------------------------------------------------------------
-- | Create new instance in Redis.
create :: Handler b Redson ()
create = ifTop $ do
  -- Parse request body to list of pairs
  -- @todo Use readRequestBody
  j <- jsonToHsetall <$> getRequestBody
  when (isNothing j)
       serverError

  model <- getModelName
  newId <- runRedisDB database $ do
    -- Take id from global:model:id
    Right n <- incr $ modelIdKey model
    newId <- return $ (BU.fromString . show) n

    -- Save new instance
    _ <- hmset (modelKey model newId) (fromJust j) 
    _ <- lpush (modelTimeline model) [newId]
    return newId

  ps <- gets _events

  liftIO $ PS.publish ps $ creationMessage model newId

  -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5:
  --
  -- the response SHOULD be 201 (Created) and contain an entity which
  -- describes the status of the request and refers to the new
  -- resource
  modifyResponse $ (setContentType "application/json" . setResponseCode 201)
  -- Tell client new instance id in response JSON.
  writeLBS $ A.encode $ M.fromList $ ("id", newId):(fromJust j)
  return ()


------------------------------------------------------------------------------
-- | Read instance from Redis.
read' :: HasHeist b => Handler b Redson ()
read' = ifTop $ do
  -- Pass to index page handler (Snap routing bug workaround)
  id <- fromParam "id"
  when (B.null id)
       pass

  key <- getModelKey
  r <- runRedisDB database $ do
    Right r <- hgetall key
    return r

  when (null r)
       notFound

  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson r)
  return ()

------------------------------------------------------------------------------
-- | Serve list of 10 latest instances stored in Redis.
--
-- @todo Adjustable item limit.
timeline :: HasHeist b => Handler b Redson ()
timeline = ifTop $ do
  model <- getModelName

  r <- runRedisDB database $ do
    Right r <- lrange (modelTimeline model) 0 9
    return r

  modifyResponse $ setContentType "application/json"
  writeLBS (enc' r)
    where
        enc' :: [B.ByteString] -> BZ.ByteString
        enc' r = A.encode r


------------------------------------------------------------------------------
-- | WebSockets handler which pushes instance creation/deletion events
-- to client.
modelEvents :: HasHeist b => Handler b Redson ()
modelEvents = ifTop $ do
  ps <- gets _events
  liftSnap $ runWebSocketsSnap (\r -> do
                                  acceptRequest r
                                  PS.subscribe ps)
  

------------------------------------------------------------------------------
-- | Update existing instance in Redis.
update :: Handler b Redson ()
update = ifTop $ do
  j <- jsonToHsetall <$> getRequestBody
  when (isNothing j)
       serverError

  key <- getModelKey
  runRedisDB database $ hmset key (fromJust j)
  modifyResponse $ setResponseCode 204
  return()

------------------------------------------------------------------------------
-- | Delete instance from Redis (including timeline).
delete :: Handler b Redson ()
delete = ifTop $ do
  id <- getModelId
  model <- getModelName
  key <- getModelKey

  r <- runRedisDB database $ do
    -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7
    --
    -- A successful response SHOULD be 200 (OK) if the response includes
    -- an entity describing the status
    Right r <- hgetall key
    return r

  when (null r)
       notFound

  runRedisDB database $ lrem (modelTimeline model) 1 id >> del [key]

  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson r)
  
  ps <- gets _events
  liftIO $ PS.publish ps $ deletionMessage model id


-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: HasHeist b => [(B.ByteString, Handler b Redson ())]
routes = [ (":model/", method GET emptyForm)
         , (":model/model", method GET metamodel)
         , (":model/timeline", method GET timeline)
         , (":model/events", modelEvents)
         , (":model", method POST create)
         , (":model/:id", method GET read')
         , (":model/:id", method PUT update)
         , (":model/:id", method DELETE delete)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redboneInit :: HasHeist b => SnapletInit b Redson
redboneInit = makeSnaplet "redbone" "Backbone.js backend with Redis storage" Nothing $
          do
            r <- nestSnaplet "" database $ redisDBInit defaultConnectInfo
            p <- liftIO PS.newPubSub
            addRoutes routes
            return $ Redson r p
