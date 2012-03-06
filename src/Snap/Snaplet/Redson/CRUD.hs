{-# LANGUAGE OverloadedStrings #-}

{-|

Snap-agnostic low-level CRUD operations. 

(This module may be refactored to a separate package.)

This module may be used for batch uploading of database data.

-}
module Snap.Snaplet.Redson.CRUD
    ( -- * CRUD operations
      create
    , update
    , delete
    -- * Redis helpers
    , InstanceId
    , instanceKey
    , modelIndex
    , modelTimeline
    )

where

import Prelude hiding (id)

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
-- | Cut instance model and id from Redis key
--
-- >>> keyToId "case:32198"
-- 32198
keyToId :: B.ByteString -> InstanceId
keyToId key = B.tail $ B.dropWhile (/= 0x3a) key


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
forIndices commit findices action =
    mapM_ (\i -> case (M.lookup i commit) of
                   Just v -> action i v
                   Nothing -> return ())
        findices


------------------------------------------------------------------------------
-- | Create reverse indices for new commit.
createIndices :: ModelName 
              -> InstanceId
              -> Commit 
              -> [FieldName]               -- ^ Index fields
              -> Redis ()
createIndices mname id commit findices =
    forIndices commit findices $
                   \i v -> when (v /= "") $
                           sadd (modelIndex mname i v) [id] >> return ()


------------------------------------------------------------------------------
-- | Remove indices previously created by commit (should contain all
-- indexed fields only).
deleteIndices :: ModelName 
              -> InstanceId                -- ^ Instance id.
              -> [(FieldName, FieldValue)] -- ^ Commit with old
                                           -- indexed values (zipped
                                           -- from HMGET).
              -> Redis ()
deleteIndices mname id commit =
    mapM_ (\(i, v) -> srem (modelIndex mname i v) [id])
          commit


------------------------------------------------------------------------------
-- | Create new instance in Redis and indices for it.
--
-- Bump model id counter and update timeline, return new instance id.
--
-- TODO: Support pubsub from here
create :: ModelName           -- ^ Model name
       -> Commit              -- ^ Key-values of instance data
       -> [FieldName]         -- ^ Index fields
       -> Redis (Either Error InstanceId)
create mname commit findices = do
  -- Take id from global:model:id
  Right n <- incr $ modelIdKey mname
  newId <- return $ (BU.fromString . show) n

  -- Save new instance
  _ <- hmset (instanceKey mname newId) (M.toList commit)
  _ <- lpush (modelTimeline mname) [newId]

  createIndices mname newId commit findices
  return (Right newId)


------------------------------------------------------------------------------
-- | Modify existing instance in Redis, updating indices
--
-- TODO: Handle non-existing instance as error here?
update :: ModelName
       -> InstanceId
       -> Commit
       -> [FieldName]
       -> Redis (Either Error ())
update mname id commit findices = 
  let
      key = instanceKey mname id
      unpacked = M.toList commit
      newFields = map fst unpacked
  in do
    Right old <- hmget key findices
    hmset key unpacked

    deleteIndices mname id $ 
                  zip (filter (flip elem newFields) findices)
                      (catMaybes old)
    createIndices mname id commit findices

    return (Right ())

------------------------------------------------------------------------------
-- | Remove existing instance in Redis, cleaning up old indices.
--
-- Does not check if instance exists.
delete :: ModelName
       -> InstanceId
       -> [FieldName]
       -> Redis (Either Error ())
delete mname id findices = 
    let
        key = instanceKey mname id
    in do
      Right old <- hmget key findices
      lrem (modelTimeline mname) 1 id >> del [key]
      deleteIndices mname id (zip findices (catMaybes old))
      return (Right ())
