{-# LANGUAGE OverloadedStrings #-}

{-|

Snap-agnostic low-level CRUD operations.

This module may be used for batch uploading of database data.

-}
module Snap.Snaplet.Redson.CRUD

where

import Control.Monad.State
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.Map as M

import Database.Redis

import Snap.Snaplet.Redson.Metamodel
import Snap.Snaplet.Redson.Util


type InstanceId = B.ByteString


------------------------------------------------------------------------------
-- | Build Redis key given model name and instance id
instanceKey :: ModelName -> InstanceId -> B.ByteString
instanceKey model id = B.concat [model, ":", id]


------------------------------------------------------------------------------
-- | Get Redis key which stores id counter for model
modelIdKey :: ModelName -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]


------------------------------------------------------------------------------
-- | Get Redis key which stores timeline for model
modelTimeline :: ModelName -> B.ByteString
modelTimeline model = B.concat ["global:", model, ":timeline"]


------------------------------------------------------------------------------
-- | Build Redis key for field index of model.
modelIndex :: ModelName
           -> B.ByteString -- ^ Field name
           -> B.ByteString -- ^ Field value
           -> B.ByteString
modelIndex model field value = B.concat [model, ":", field, ":", value]


------------------------------------------------------------------------------
-- | Perform provided action for every indexed field in commit.
--
-- Action is called with index field name and its value in commit.
forIndices :: Commit 
           -> [FieldName] 
           -> (FieldName -> FieldValue -> Redis ())
           -> Redis ()
forIndices commit indices action =
    mapM_ (\i -> case (M.lookup i commit) of
                   Just v -> action i v)
        indices


------------------------------------------------------------------------------
-- | Create reverse indices for new commit.
createIndices :: ModelName 
              -> InstanceId
              -> Commit 
              -> [FieldName]               -- ^ Index fields
              -> Redis ()
createIndices name id commit indices =
    forIndices commit indices $
                   \i v -> when (v /= "") $
                           sadd (modelIndex name i v) [id] >> return ()


------------------------------------------------------------------------------
-- | Remove indices previously created by commit (should contain all
-- indexed fields only).
deleteIndices :: ModelName 
              -> InstanceId                -- ^ Instance id.
              -> [(FieldName, FieldValue)] -- ^ Commit with old
                                           -- indexed values (zipped
                                           -- from HMGET).
              -> [FieldName]               -- ^ Index fields
              -> Redis ()
deleteIndices name id commit indices =
    mapM_ (\(i, v) -> srem (modelIndex name i v) [id])
          commit


------------------------------------------------------------------------------
-- | Create new instance in Redis.
--
-- Bump model id counter and update timeline, return new instance id.
--
-- TODO: Support pubsub from here
create :: ModelName           -- ^ Model name
       -> Commit              -- ^ Key-values of instance data
       -> [FieldName]         -- ^ Index fields
       -> Redis (Either Error B.ByteString)
create name commit indices = do
  -- Take id from global:model:id
  Right n <- incr $ modelIdKey name
  newId <- return $ (BU.fromString . show) n

  -- Save new instance
  _ <- hmset (instanceKey name newId) (M.toList commit)
  _ <- lpush (modelTimeline name) [newId]

  -- Create indices
  createIndices name newId commit indices
  return (Right newId)


------------------------------------------------------------------------------
-- | Modify existing instance in Redis.
--
-- TODO: Handle non-existing instance as error here?
update :: ModelName
       -> InstanceId
       -> Commit
       -> [FieldName]
       -> Redis (Either Error ())
update name id commit indices = 
  let
      key = instanceKey name id
  in do
    Right old <- hmget key indices
    hmset key (M.toList commit)

    deleteIndices name id (zip indices (catMaybes old)) indices
    createIndices name id commit indices
    return (Right ())
