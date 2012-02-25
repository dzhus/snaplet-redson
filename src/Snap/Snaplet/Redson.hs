{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

CRUD for JSON data with Redis storage.

Can be used as Backbone.sync backend.

-}

module Snap.Snaplet.Redson (Redson
                           , redsonInit)
where

import Prelude hiding (concat)

import Control.Monad.State
import Control.Monad.Trans
import Data.Functor

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
import Snap.Snaplet.Auth
import Snap.Snaplet.RedisDB
import Snap.Util.FileServe

import Network.WebSockets
import Network.WebSockets.Snap
import qualified Network.WebSockets.Util.PubSub as PS

import Database.Redis

import Snap.Snaplet.Redson.Util


------------------------------------------------------------------------------
-- | Redson snaplet state type.
data Redson b = Redson
             { _database :: Snaplet RedisDB
             , _events :: PS.PubSub Hybi10
             , _auth :: Lens b (Snaplet (AuthManager b))
             }

makeLens ''Redson


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
-- | Builder for WebSockets message containing JSON describing
-- creation or deletion of model instance.
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
-- Note using explicit B.ByteString type over BS s as suggested by
-- redis because BS s doesn't imply ToJSON s.
hgetallToJson :: [(B.ByteString, B.ByteString)] -> BZ.ByteString
hgetallToJson r = A.encode $ M.fromList r


------------------------------------------------------------------------------
-- | Decode B.ByteString with JSON to list of hash keys & values for
-- Redis HMSET
--
-- Return Nothing if parsing failed.
jsonToHmset :: BZ.ByteString -> Maybe [(B.ByteString, B.ByteString)]
jsonToHmset s =
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
-- 
-- *TODO*: Use readRequestBody
create :: Handler b (Redson b) ()
create = ifTop $ do
  -- Parse request body to list of pairs
  j <- jsonToHmset <$> getRequestBody
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
read' :: Handler b (Redson b) ()
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
-- *TODO*: Adjustable item limit.
timeline :: Handler b (Redson b) ()
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
modelEvents :: Handler b (Redson b) ()
modelEvents = ifTop $ do
  ps <- gets _events
  liftSnap $ runWebSocketsSnap (\r -> do
                                  acceptRequest r
                                  PS.subscribe ps)
  

------------------------------------------------------------------------------
-- | Update existing instance in Redis.
-- 
-- *TODO* Report 201 if previously existed
update :: Handler b (Redson b) ()
update = ifTop $ do
  j <- jsonToHmset <$> getRequestBody
  when (isNothing j)
       serverError

  key <- getModelKey
  runRedisDB database $ hmset key (fromJust j)
  modifyResponse $ setResponseCode 204
  return()


------------------------------------------------------------------------------
-- | Delete instance from Redis (including timeline).
delete :: Handler b (Redson b) ()
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
routes :: [(B.ByteString, Handler b (Redson b) ())]
routes = [ (":model/timeline", method GET timeline)
         , (":model/events", modelEvents)
         , (":model", method POST create)
         , (":model/:id", method GET read')
         , (":model/:id", method PUT update)
         , (":model/:id", method DELETE delete)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redsonInit :: Lens b (Snaplet (AuthManager b))
           -> SnapletInit b (Redson b)
redsonInit topAuth = makeSnaplet 
                     "redson" 
                     "CRUD for JSON data with Redis storage" 
                     Nothing $
          do
            r <- nestSnaplet "db" database $ redisDBInit defaultConnectInfo
            p <- liftIO PS.newPubSub
            addRoutes routes
            return $ Redson r p topAuth
