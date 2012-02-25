{-# LANGUAGE OverloadedStrings #-}

-- | Metamodel partial parser which allows to extract field
-- permissions data.

module Snap.Snaplet.Redson.Metamodel

where

import Control.Applicative
import Data.Functor
import Data.List

import Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8

import Snap.Snaplet.Auth


-- | Field name.
type FieldName = B.ByteString

-- | Field value.
type FieldValue = B.ByteString

-- | List of field key-value pairs.
--
-- Suitable for using with 'Database.Redis.hmset'.
type Commit = [(FieldName, FieldValue)]


-- | Description of field set.
data Metamodel = Metamodel { fields :: [Field] }
                 deriving Show


data Field = Field { name :: FieldName
                   , canRead :: [Role]
                   , canWrite :: [Role]}
             deriving Show


instance FromJSON Metamodel where
    parseJSON (Object v) = Metamodel <$>
                           v .: "fields"
    parseJSON _          = error "Unexpected input"


instance FromJSON Field where
    parseJSON (Object v) = Field <$>
                           v .: "name" <*>
                           v .:? "canRead" .!= [] <*>
                           v .:? "canEdit" .!= []
    parseJSON _          = error "Unexpected input"


-- | Get list of metamodel fields writable by user.
--
-- TODO: Cache this.
getWritableFields :: AuthUser -> Metamodel -> [FieldName]
getWritableFields user model =
    let
        writeRoles :: Field -> [Role]
        writeRoles f = union (canRead f) (canWrite f)
    in
      map name $
          filter (\field -> not $
                            null $
                            intersect (writeRoles field) (userRoles user))
                     (fields model)

-- | Get list of metamodel fields readable by user.
--
-- TODO: Cache this.
getReadableFields :: AuthUser -> Metamodel -> [FieldName]
getReadableFields user model =
    let
        readRoles :: Field -> [Role]
        readRoles f = canRead f
    in
      map name $
          filter (\field -> not $
                            null $
                            intersect (readRoles field) (userRoles user))
                     (fields model)


-- | Check permissions to write the given set of metamodel fields.
checkWrite :: AuthUser -> Metamodel -> Commit -> Bool
checkWrite user model commit =
    let
        writables = getWritableFields user model
    in
      null $ intersect (map fst commit) writables


-- | Filter out commit fields which are not readable by user.
filterUnreadable :: AuthUser -> Metamodel -> Commit -> Commit
filterUnreadable user model commit =
    let
        readables = getReadableFields user model
    in
      filter (\(k, v) -> elem k readables) commit
