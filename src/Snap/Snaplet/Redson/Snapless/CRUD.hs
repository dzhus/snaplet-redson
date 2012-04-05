{-# LANGUAGE OverloadedStrings #-}

{-|

Snap-agnostic low-level CRUD operations. No model definitions are used
on this level. Instead, objects must be 

This module may be used for batch uploading of database data.

-}
module Snap.Snaplet.Redson.Snapless.CRUD
    ( -- * CRUD operations
      create
    , update
    , delete
    -- * Redis helpers
    , InstanceId
    , instanceKey
    , modelIndex
    , modelTimeline
    , collate
    , onlyFields
    )

where

import Prelude hiding (id)

import Control.Monad.State
import Data.Functor
import Data.Maybe

import Data.Char
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.Map as M

import Database.Redis

import Snap.Snaplet.Redson.Snapless.Metamodel


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
-- | Strip value of punctuation, spaces, convert all to lowercase.
collate :: FieldValue -> FieldValue
collate = E.encodeUtf8 . T.toLower .
          (T.filter (\c -> (not (isSpace c || isPunctuation c)))) .
          E.decodeUtf8


------------------------------------------------------------------------------
-- | Perform provided action for every indexed field in commit.
--
-- Action is called with index field name and its value in commit.
forIndices :: Commit
           -> [FieldIndex]
           -> (FieldName -> FieldValue -> Redis ())
           -> Redis ()
forIndices commit findices action =
    mapM_ (\i -> case (M.lookup i commit) of
                   Just v -> action i v
                   Nothing -> return ())
    (fst <$> findices)


------------------------------------------------------------------------------
-- | Create reverse indices for new commit.
createIndices :: ModelName
              -> InstanceId
              -> Commit
              -> [FieldIndex]
              -> Redis ()
createIndices mname id commit findices =
    forIndices commit findices $
                   \i rawVal -> 
                       let 
                           v = collate rawVal
                       in
                         when (v /= "") $
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
-- | Get old values of index fields stored under key.
getOldIndices :: B.ByteString -> [FieldName] -> Redis [Maybe B.ByteString]
getOldIndices key findices = do
  reply <- hmget key findices
  return $ case reply of
             Left _ -> []
             Right l -> l


------------------------------------------------------------------------------
-- | Extract values of named fields from commit.
onlyFields :: Commit -> [FieldName] -> [Maybe FieldValue]
onlyFields commit names = map (flip M.lookup commit) names


------------------------------------------------------------------------------
-- | Create new instance in Redis and indices for it.
--
-- Bump model id counter and update timeline, return new instance id.
--
-- TODO: Support pubsub from here
create :: ModelName           -- ^ Model name
       -> Commit              -- ^ Key-values of instance data
       -> [FieldIndex]
       -> Redis (Either Reply InstanceId)
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
       -> [FieldIndex]
       -> Redis (Either Reply ())
update mname id commit findices =
  let
      key = instanceKey mname id
      unpacked = M.toList commit
      newFields = map fst unpacked
      indnames = fst <$> findices
  in do
    old <- getOldIndices key indnames
    hmset key unpacked

    deleteIndices mname id $
                  zip (filter (flip elem newFields) indnames)
                      (catMaybes old)
    createIndices mname id commit findices

    return (Right ())

------------------------------------------------------------------------------
-- | Remove existing instance in Redis, cleaning up old indices.
--
-- Does not check if instance exists.
delete :: ModelName
       -> InstanceId
       -> [FieldIndex]
       -> Redis (Either Reply ())
delete mname id findices =
    let
        key = instanceKey mname id
        indnames = fst <$> findices
    in do
      old <- getOldIndices key indnames
      lrem (modelTimeline mname) 1 id >> del [key]
      deleteIndices mname id (zip indnames (catMaybes old))
      return (Right ())
