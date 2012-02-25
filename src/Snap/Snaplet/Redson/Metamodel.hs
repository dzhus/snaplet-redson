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


type MetamodelName = B.ByteString

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


-- | Get lists of metamodel fields which are readable and writable by
-- given user.
--
-- TODO: Cache this.
getFieldPermissions :: AuthUser -> Metamodel -> ([FieldName], [FieldName])
getFieldPermissions user model =
    let
        -- Get names of metamodel fields for which the given function
        -- has non-null intersection with user roles
        getFields getRoles =
            map name $
                filter (\field -> not $ null $
                                  intersect (getRoles field) (userRoles user))
                    (fields model)
    in
      (getFields canRead, getFields (\f -> union (canRead f) (canWrite f)))


-- | Check permissions to write the given set of metamodel fields.
checkWrite :: AuthUser -> Metamodel -> Commit -> Bool
checkWrite user model commit =
    let
        writables = snd $ getFieldPermissions user model
    in
      null $ intersect (map fst commit) writables


-- | Filter out commit fields which are not readable by user.
filterUnreadable :: AuthUser -> Metamodel -> Commit -> Commit
filterUnreadable user model commit =
    let
        readables = fst $ getFieldPermissions user model
    in
      filter (\(k, v) -> elem k readables) commit
