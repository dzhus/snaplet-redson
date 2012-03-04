{-# LANGUAGE OverloadedStrings #-}

{-|

Snap-agnostic low-level CRUD operations.

This module may be used for batch uploading of database data.

|-}
module Snap.Snaplet.Redson.CRUD

where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU (fromString)
import Database.Redis

import Snap.Snaplet.Redson.Metamodel


------------------------------------------------------------------------------
-- | Build Redis key given model name and id
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
-- | Create new instance in Redis.
--
-- Bump model id counter and update timeline, return new instance id.
--
-- TODO: Support pubsub from here
create :: ModelName -> Commit -> Redis B.ByteString
create name j = do
  -- Take id from global:model:id
  Right n <- incr $ modelIdKey name
  newId <- return $ (BU.fromString . show) n

  -- Save new instance
  _ <- hmset (instanceKey name newId) j
  _ <- lpush (modelTimeline name) [newId]
  return newId
