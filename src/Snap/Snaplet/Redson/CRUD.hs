{-# LANGUAGE OverloadedStrings #-}

{-|

Snap-agnostic low-level CRUD operations.

This module may be used for batch uploading of database data.

|-}
module Snap.Snaplet.Redson.CRUD

where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.Map as M

import Database.Redis

import Snap.Snaplet.Redson.Metamodel


------------------------------------------------------------------------------
-- | Build Redis key given model name and instance id
instanceKey :: B.ByteString -> B.ByteString -> B.ByteString
instanceKey model id = B.concat [model, ":", id]


------------------------------------------------------------------------------
-- | Get Redis key which stores id counter for model
modelIdKey :: B.ByteString -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]


------------------------------------------------------------------------------
-- | Get Redis key which stores timeline for model
modelTimeline :: B.ByteString -> B.ByteString
modelTimeline model = B.concat ["global:", model, ":timeline"]


------------------------------------------------------------------------------
-- | Build Redis key for field index of model.
modelIndex :: B.ByteString -> B.ByteString -> B.ByteString
modelIndex model field = B.concat [model, ":", field]


------------------------------------------------------------------------------
-- | Create new instance in Redis.
--
-- Bump model id counter and update timeline, return new instance id.
--
-- TODO: Support pubsub from here
create :: ModelName           -- ^ Model id
       -> Commit              -- ^ Key-values of instance data
       -> [FieldName]         -- ^ Index fields
       -> Redis B.ByteString
create name j indices = do
  -- Take id from global:model:id
  Right n <- incr $ modelIdKey name
  newId <- return $ (BU.fromString . show) n

  -- Save new instance
  _ <- hmset (instanceKey name newId) (M.toList j)
  _ <- lpush (modelTimeline name) [newId]

  return newId
