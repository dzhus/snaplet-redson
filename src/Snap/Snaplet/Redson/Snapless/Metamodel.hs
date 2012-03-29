{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model definition parser, served model routines.

module Snap.Snaplet.Redson.Snapless.Metamodel

where

import Control.Applicative

import Data.Aeson
import qualified Data.ByteString as B

import Data.Lens.Common
import Data.Lens.Template
import Data.List
import Data.Maybe

import qualified Data.Map as M


type ModelName = B.ByteString

type FieldName = B.ByteString

type FieldValue = B.ByteString

-- | Name of indexed field and collation flag.
type FieldIndex = (FieldName, Bool)


-- | List of field key-value pairs.
--
-- Suitable for using with 'Database.Redis.hmset'.
type Commit = M.Map FieldName FieldValue


-- | Field permissions property.
data Permissions = Roles [B.ByteString]
                 | Everyone
                 | Nobody
                 deriving Show


data FieldTargets = Fields [FieldName]
                  | AllFields
                  | NoneFields
                  deriving Show


-- | Map of field annotations which are transparently handled by
-- server without any logic.
type FieldMeta = M.Map FieldName Value


-- | Form field object.
data Field = Field { name           :: FieldName
                   , fieldType      :: B.ByteString
                   , index          :: Bool
                   , indexCollate   :: Bool
                   , groupName      :: Maybe B.ByteString
                   , meta           :: Maybe FieldMeta
                   , _canRead       :: Permissions
                   , _canWrite      :: Permissions
                   }
             deriving Show

makeLenses [''Field]


-- | A list of properties to be applied to named fields.
data Application = Application { targets    :: FieldTargets
                               , apMeta     :: Maybe FieldMeta
                               , _apRead    :: Maybe Permissions
                               , _apWrite   :: Maybe Permissions
                               }
                   deriving Show

makeLenses [''Application]


-- | Model describes fields and permissions.
--
-- Models are built from JSON definitions (using FromJSON instance for
-- Model) with further group splicing ('spliceGroups'), applications
-- ('doApplications') and index caching ('cacheIndices').
data Model = Model { modelName      :: ModelName
                   , title          :: B.ByteString
                   , fields         :: [Field]
                   , applications   :: [Application]
                   , _canCreateM    :: Permissions
                   , _canReadM      :: Permissions
                   , _canUpdateM    :: Permissions
                   , _canDeleteM    :: Permissions
                   , indices        :: [FieldIndex]
                   -- ^ Cached list of index fields.
                   }
             deriving Show

makeLenses [''Model]

-- | Used when field type is not specified in model description.
defaultFieldType :: B.ByteString
defaultFieldType = "text"


instance FromJSON Model where
    parseJSON (Object v) = Model          <$>
        v .: "name"                       <*>
        v .: "title"                      <*>
        v .: "fields"                     <*>
        v .:? "applications" .!= []       <*>
        v .:? "canCreate" .!= Nobody      <*>
        v .:? "canRead"   .!= Nobody      <*>
        v .:? "canUpdate" .!= Nobody      <*>
        v .:? "canDelete" .!= Nobody      <*>
        pure []
    parseJSON _          = error "Could not parse model description"

instance ToJSON Model where
    toJSON mdl = object
      [ "name"       .= modelName mdl
      , "title"      .= title mdl
      , "fields"     .= fields mdl
      , "indices"    .= indices mdl
      , "canCreate"  .= _canCreateM mdl
      , "canRead"    .= _canReadM mdl
      , "canUpdate"  .= _canUpdateM mdl
      , "canDelete"  .= _canDeleteM mdl
      ]


instance FromJSON Permissions where
    parseJSON (Bool True)  = return Everyone
    parseJSON (Bool False) = return Nobody
    parseJSON v@(Array _)  = Roles <$> parseJSON v
    parseJSON _            = error "Could not permissions"

instance ToJSON Permissions where
    toJSON Everyone  = Bool True
    toJSON Nobody    = Bool False
    toJSON (Roles r) = toJSON r


instance FromJSON Field where
    parseJSON (Object v) = Field        <$>
      v .: "name"                       <*>
      v .:? "type" .!= defaultFieldType <*>
      v .:? "index"        .!= False    <*>
      v .:? "indexCollate" .!= False    <*>
      v .:? "groupName"                 <*>
      v .:? "meta"                      <*>
      v .:? "canRead"  .!= Nobody       <*>
      v .:? "canWrite" .!= Nobody
    parseJSON _          = error "Could not parse field properties"

instance ToJSON Field where
    toJSON f = object
      [ "name"          .= name f
      , "type"          .= fieldType f
      , "index"         .= index f
      , "indexCollate"  .= indexCollate f
      , "groupName"     .= groupName f
      , "canRead"       .= _canRead f
      , "canWrite"      .= _canWrite f
      , "meta"          .= meta f
      ]


instance FromJSON FieldTargets where
    parseJSON (Bool True)  = return AllFields
    parseJSON (Bool False) = return NoneFields
    parseJSON v@(Array _)  = Fields <$> parseJSON v
    parseJSON _            = error "Could not application targets"


instance FromJSON Application where
    parseJSON (Object v) = Application  <$>
      v .:? "targets" .!= NoneFields    <*>
      v .:? "meta"                      <*>
      v .:? "canRead"                   <*>
      v .:? "canWrite"
    parseJSON _          = error "Could not parse application entry"


type Groups = M.Map B.ByteString [Field]


-- | Build new name `f_gK` for every field of group `g` to which field
-- `f` is spliced into.
groupFieldName :: FieldName
               -- ^ Name of field which is spliced into group
               -> FieldName
               -- ^ Name of group field
               -> FieldName
groupFieldName parent field = B.concat [parent, "_", field]


-- | Replace all model fields having `group` type with actual group
-- fields.
spliceGroups :: Groups -> Model -> Model
spliceGroups groups model =
    let
        origFields = fields model
    in
      model{fields = concat $
            map (\f -> 
                 case groupName f of
                   Just n -> 
                       case (M.lookup n groups) of
                         Just grp -> 
                             map (\gf -> gf{ groupName = Just n
                                           , name = groupFieldName (name f) (name gf)
                                           }) grp
                         Nothing -> [f]
                   _ -> [f]
                ) origFields}


-- | Perform all applications in model.
doApplications :: Model -> Model
doApplications model =
    let
        -- Update values in old meta with those specified in
        -- application meta
        mergeFieldsMeta :: Maybe FieldMeta -> Field -> Field
        mergeFieldsMeta (Just patchMeta) original =
            let 
                oldMeta = fromMaybe M.empty (meta original)
                -- TODO Monoid is out there
                newMeta =
                    M.foldlWithKey' (\o k v -> M.insert k v o) oldMeta patchMeta
            in
              original{meta = Just newMeta}
        mergeFieldsMeta Nothing original = original

        -- Try to perform application for fields in list.
        processField :: [Field] -> Application -> [Field]
        processField (f:fs) ap =
            let
                -- List of setters to apply to field which will update
                -- it with application values
                patchBits :: [Field -> Field]
                patchBits = [mergeFieldsMeta (apMeta ap)] ++
                          map (\(from, to) -> 
                                   maybe id (to ^=) (ap ^. from))
                          [ (apRead,  canRead)
                          , (apWrite, canWrite)
                          ]
                patch = foldl1' (.) patchBits
                -- Meta field is merged separately
                newF = case targets ap of
                         AllFields -> patch f
                         Fields ts -> if (elem (name f) ts)
                                      then patch f
                                      else f
                         _ -> f
            in
              newF:(processField fs ap)
        processField [] _ = []
    in
      model{fields = foldl' processField (fields model) (applications model)}


-- | Set indices field of model to list of 'FieldIndex'es
cacheIndices :: Model -> Model
cacheIndices model = 
    let
        maybeCacheIndex indexList field =
            case (index field, indexCollate field) of
              (True, c) -> (name field, c):indexList
              _ -> indexList
    in
      model{indices = foldl' maybeCacheIndex [] (fields model)}
