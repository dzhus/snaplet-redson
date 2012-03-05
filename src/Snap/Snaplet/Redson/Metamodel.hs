{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Lens.Common
import Data.Lens.Template

import qualified Data.Map as M

import Snap.Core (Method(..))
import Snap.Snaplet.Auth


type ModelName = B.ByteString

-- | Field name.
type FieldName = B.ByteString

-- | Field value.
type FieldValue = B.ByteString

-- | List of field key-value pairs.
--
-- Suitable for using with 'Database.Redis.hmset'.
type Commit = M.Map FieldName FieldValue


-- | Field permissions property.
data Permissions = Roles [Role]
                 | Everyone
                 | Nobody
                 deriving Show


-- | Form field object.
data Field = Field { name           :: FieldName
                   , fieldType      :: B.ByteString
                   , label          :: Maybe B.ByteString
                   , choice         :: Maybe [FieldValue]
                   , defaultVal     :: Maybe Value
                   , index          :: Bool
                   , required       :: Maybe Bool
                   , invisible      :: Maybe Bool
                   , referencables  :: Maybe [ModelName]
                   , canRead        :: Permissions
                   , canWrite       :: Permissions
                   }
             deriving Show


-- | Model describes fields and permissions.
data Model = Model { title          :: B.ByteString
                   , fields         :: [Field]
                   , _canCreateF    :: Permissions
                   , _canReadF      :: Permissions
                   , _canUpdateF    :: Permissions
                   , _canDeleteF    :: Permissions
                   , indices        :: [FieldName]
                   -- ^ Cached list of index fields.
                   }
                 deriving Show

makeLenses [''Model]


-- | Used when field type is not specified in model description.
defaultFieldType :: B.ByteString
defaultFieldType = "text"


instance FromJSON Model where
    parseJSON (Object v) = do
      fields <- parseJSON =<< (v .: "fields")
      return =<< Model                    <$>
        v .: "title"                      <*>
        v .: "fields"                     <*>
        v .:? "canCreate" .!= Nobody      <*>
        v .:? "canRead"   .!= Nobody      <*>
        v .:? "canUpdate" .!= Nobody      <*>
        v .:? "canDelete" .!= Nobody      <*>
        (pure $ map name $ filter index fields)
    parseJSON _          = error "Could not parse model description"

instance ToJSON Model where
    toJSON mdl = object
      [ "title"      .= title mdl
      , "fields"     .= fields mdl
      , "indices"    .= indices mdl
      , "canCreate"  .= _canCreateF mdl
      , "canRead"    .= _canReadF mdl
      , "canUpdate"  .= _canUpdateF mdl
      , "canDelete"  .= _canDeleteF mdl
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
      v .:? "index"    .!= False        <*>
      v .:? "required"                  <*>
      v .:? "invisible"                 <*>
      v .:? "reference-models"          <*>
      v .:? "canRead"  .!= Nobody       <*>
      v .:? "canWrite" .!= Nobody
    parseJSON _          = error "Could not parse field properties"

instance ToJSON Field where
    toJSON f = object
      [ "name"          .= name f
      , "type"          .= fieldType f
      , "label"         .= label f
      , "choice"        .= choice f
      , "default"       .= defaultVal f
      , "index"         .= index f
      , "required"      .= required f
      , "invisible"     .= invisible f
      , "canRead"       .= canRead f
      , "canWrite"      .= canWrite f
      , "referencables" .= referencables f
      ]


-- | User who has all permissions (used in security-disabled mode).
data SuperUser = SuperUser

-- | Either superuser or logged in user.
type User = Either SuperUser AuthUser

-- | Map between CRUD methods and form permission lenses.
methodMap :: [(Method, Lens Model Permissions)]
methodMap = [ (POST,   canCreateF)
            , (GET,    canReadF)
            , (PUT,    canUpdateF)
            , (DELETE, canDeleteF)
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
-- 'SuperUser' can read and write all fields.
--
-- TODO: Cache this.
getFieldPermissions :: User -> Model -> ([FieldName], [FieldName])
getFieldPermissions (Left SuperUser) model =
    let
        f = map name $ fields model
    in
      (f, f)
getFieldPermissions (Right user) model =
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


-- | Get list of CRUD/HTTP methods accessible by user for model.
--
-- 'SuperUser' has all methods.
--
-- POST permission implies PUT.
--
-- TODO: Cache this.
getModelPermissions :: User -> Model -> [Method]
getModelPermissions (Left SuperUser) _ = [POST, GET, PUT, DELETE]
getModelPermissions (Right user) model =
    let
        askPermission perm = intersectPermissions
                             (model ^. perm)
                             (userRoles user)
        rawPerms = map fst $
                   filter (\(m, p) -> askPermission p) methodMap
    in
      if (elem POST rawPerms)
      then rawPerms ++ [PUT]
      else rawPerms


-- | Check permissions to write the given set of model fields.
--
-- 'SuperUser' can always write to any set of fields. When there's no
-- model, always succeed.
checkWrite :: User -> (Maybe Model) -> Commit -> Bool
checkWrite (Left SuperUser) _           _ = True
checkWrite _                Nothing     _ = True
checkWrite user@(Right _)  (Just model) commit =
    let
        writables = snd $ getFieldPermissions user model
        commitFields = M.keys commit
    in
      all (flip elem writables) commitFields


-- | Filter out commit fields which are not readable by user.
--
-- 'SuperUser' can always read all fields.
filterUnreadable :: User -> Maybe Model -> Commit -> Commit
filterUnreadable (Left SuperUser) _           commit = commit
filterUnreadable _                Nothing     commit = commit
filterUnreadable user@(Right _)  (Just model) commit =
    let
        readables = fst $ getFieldPermissions user model
    in
      M.filterWithKey (\k _ -> elem k readables) commit


-- | Filter out unreadable fields from model description, set
-- per-field "canEdit" to boolean depending on current user's
-- permissions, set whole-form C-R-U-D permissions to booleans in
-- similar fashion.
stripModel :: User -> Model -> Model
stripModel user model =
    let
        -- To set permission value to boolean depending on user roles
        stripMapper :: Bool -> Permissions
        stripMapper b = if b then Everyone else Nobody
        (readables, writables) = getFieldPermissions user model
        -- Only fields readable by current user
        readableFields = filter 
                         (\f -> elem (name f) readables)
                         (fields model)
        -- Fields with boolean canWrite's
        strippedFields = map (\f -> f{canWrite = stripMapper $
                                      elem (name f) writables})
                         readableFields
        formPerms = getModelPermissions user model
        -- List of lens setters to be applied to model
        boolFormPerms = map (\(m, p) ->
                             p ^= (stripMapper $ elem m formPerms)) 
                        methodMap
    in
      foldl' (\m f -> f m) model{ fields = strippedFields } boolFormPerms
