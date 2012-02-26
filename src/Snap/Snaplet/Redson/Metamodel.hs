{-# LANGUAGE OverloadedStrings #-}

-- | Model partial parser which allows to extract field
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


type ModelName = B.ByteString

-- | Field name.
type FieldName = B.ByteString

-- | Field value.
type FieldValue = B.ByteString

-- | List of field key-value pairs.
--
-- Suitable for using with 'Database.Redis.hmset'.
type Commit = [(FieldName, FieldValue)]


-- | Description of field set.
data Model = Model { title     :: B.ByteString
                   , fields    :: [Field]
                   , canReadF  :: Permissions
                   , canWriteF :: Permissions
                   }
                 deriving Show


-- | Field permissions property.
data Permissions = Roles [Role]
                 | Everyone
                 | Nobody
                 deriving Show


data Field = Field { name           :: FieldName
                   , fieldType      :: B.ByteString
                   , label          :: Maybe B.ByteString
                   , choice         :: Maybe [FieldValue]
                   , defaultVal     :: Maybe Value
                   , canRead        :: Permissions
                   , canWrite       :: Permissions
                   }
             deriving Show


-- | Used when field type is not specified in model description.
defaultFieldType :: B.ByteString
defaultFieldType = "text"


instance FromJSON Model where
    parseJSON (Object v) = Model        <$>
      v .: "title"                      <*>
      v .: "fields"                     <*>
      v .:? "canRead"  .!= Nobody       <*>
      v .:? "canWrite" .!= Nobody
    parseJSON _          = error "Could not parse model description"

instance ToJSON Model where
    toJSON mdl = object
      [ "title"      .= title mdl
      , "fields"     .= fields mdl
      , "canRead"    .= canReadF mdl
      , "canWrite"   .= canWriteF mdl
      ]


instance FromJSON Permissions where
    parseJSON (Bool True)  = return Everyone
    parseJSON (Bool False) = return Nobody
    parseJSON v@(Array r)  = Roles <$> parseJSON v
    parseJSON _            = error "Could not permissions"

instance ToJSON Permissions where
    toJSON Everyone  = Bool True
    toJSON Nobody    = Bool False
    toJSON (Roles r) = toJSON r


instance FromJSON Field where
    parseJSON (Object v) = Field        <$>
      v .: "name"                       <*>
      v .:? "type" .!= defaultFieldType <*>
      v .:? "label"                     <*>
      v .:? "choice"                    <*>
      v .:? "default"                   <*>
      v .:? "canRead"  .!= Nobody       <*>
      v .:? "canWrite" .!= Nobody
    parseJSON _          = error "Could not parse field properties"

instance ToJSON Field where
    toJSON f = object
      [ "name"       .= name f
      , "type"       .= fieldType f
      , "label"      .= label f
      , "choice"     .= choice f
      , "default"    .= defaultVal f
      , "canRead"    .= canRead f
      , "canWrite"   .= canWrite f
      ]

-- | Check if provided roles meet the permission requirements.
--
-- Always succeed in case Everyone is required, always fail in case
-- Nobody is required, otherwise succeeds when intersection is non-nil
intersectPermissions :: Permissions      -- ^ Required permissions
                     -> [Role]           -- ^ Provided roles
                     -> Bool
intersectPermissions required provided =
    case required of
      Everyone -> True
      Nobody -> False
      Roles rls -> not $ null $ intersect rls provided


-- | Get lists of metamodel fields which are readable and writable by
-- given user.
--
-- TODO: Cache this.
getFieldPermissions :: AuthUser -> Model -> ([FieldName], [FieldName])
getFieldPermissions user model =
    let
        -- Get names of metamodel fields for which the given function
        -- has non-null intersection with user roles
        getFields getRoles =
            map name $
                filter (\field -> intersectPermissions
                                  (getRoles field)
                                  (userRoles user))
                       (fields model)
    in
      (union (getFields canRead) (getFields canWrite), getFields canWrite)


-- | Get pair of booleans indicating whether model is
-- readable/writable by user.
--
-- TODO: Cache this.
getFormPermissions :: AuthUser -> Model -> (Bool, Bool)
getFormPermissions user model =
    let
        askPermission perm = intersectPermissions
                             (perm model)
                             (userRoles user)
    in
      (askPermission canReadF, askPermission canWriteF)


-- | Check permissions to write the given set of metamodel fields.
checkWrite :: AuthUser -> Model -> Commit -> Bool
checkWrite user model commit =
    let
        writables = snd $ getFieldPermissions user model
    in
      null $ intersect (map fst commit) writables


-- | Filter out commit fields which are not readable by user.
filterUnreadable :: AuthUser -> Model -> Commit -> Commit
filterUnreadable user model commit =
    let
        readables = fst $ getFieldPermissions user model
    in
      filter (\(k, v) -> elem k readables) commit

-- | Filter out unreadable fields from model description, set
-- "canEdit" to boolean depending on current user's permissions.
stripModel :: AuthUser -> Model -> Model
stripModel user model =
    let
        readables = fst $ getFieldPermissions user model
        writables = snd $ getFieldPermissions user model
    in
      model{fields = map (\f -> if elem (name f) writables
                                then f{canWrite = Everyone}
                                else f{canWrite = Nobody}) $
                     filter (\f -> elem (name f) readables) (fields model)}
