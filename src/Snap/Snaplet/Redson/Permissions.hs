-- | Metamodel bridge to Snap permissions wrt roles from
-- 'Snap.Snaplet.Auth'.

module Snap.Snaplet.Redson.Permissions
    ( SuperUser(..)
    -- * Commit checking
    , checkWrite
    , filterUnreadable
    -- * Whole-model functions
    , getModelPermissions
    , stripModel
    )

where

import Data.Lens.Common

import Data.List
import qualified Data.Map as M

import Snap.Core (Method(..))
import Snap.Snaplet.Auth


import Snap.Snaplet.Redson.Snapless.Metamodel


-- | User who has all permissions (used in security-disabled mode).
data SuperUser = SuperUser

-- | Either superuser or logged in user.
type User = Either SuperUser AuthUser


-- | Map between CRUD methods and form permission lenses.
methodMap :: [(Method, Lens Model Permissions)]
methodMap = [ (POST,   canCreateM)
            , (GET,    canReadM)
            , (PUT,    canUpdateM)
            , (DELETE, canDeleteM)
            ]


-- | Check if provided roles meet the permission requirements.
--
-- Always succeed in case Everyone is required, always fail in case
-- Nobody is required, otherwise succeeds when intersection is non-nil
--
-- We assume that Role in Snap is defined as a newtype for ByteString
-- (which is what Metamodel uses for its roles).
intersectPermissions :: Permissions      -- ^ Required permissions
                     -> [Role]           -- ^ Provided roles
                     -> Bool
intersectPermissions required provided =
    case required of
      Everyone -> True
      Nobody -> False
      Roles rls -> not $ null $ intersect rls $
                   map (\(Role r) -> r) provided


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
      (union (getFields _canRead) (getFields _canWrite), getFields _canWrite)


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
                   filter (\(_, p) -> askPermission p) methodMap
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
        strippedFields = map (\f -> f{_canWrite = stripMapper $
                                      elem (name f) writables})
                         readableFields
        formPerms = getModelPermissions user model
        -- List of lens setters to be applied to model
        boolFormPerms = map (\(m, p) ->
                             p ^= (stripMapper $ elem m formPerms)) 
                        methodMap
    in
      foldl' (\m f -> f m) model{fields = strippedFields} boolFormPerms
