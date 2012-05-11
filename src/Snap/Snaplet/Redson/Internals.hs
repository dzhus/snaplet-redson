{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Snap.Snaplet.Redson.Internals where

import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Data.Lens.Common
import Data.Lens.Template

import Network.WebSockets
import qualified Network.WebSockets.Util.PubSub as PS

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.RedisDB

import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD
import Snap.Snaplet.Redson.Snapless.Metamodel
import Snap.Snaplet.Redson.Util


type Hook b = FieldValue -> Commit -> Handler b (Redson b) Commit
type HookMap b = M.Map ModelName (M.Map FieldName [Hook b])

------------------------------------------------------------------------------
-- | Redson snaplet state type.
data Redson b = Redson
             { _database :: Snaplet RedisDB
             , auth :: Lens b (Snaplet (AuthManager b))
             , events :: PS.PubSub Hybi10
             , models :: M.Map ModelName Model
             , transparent :: Bool
             -- ^ Operate in transparent mode (not security checks).
             , hookMap :: HookMap b
             }

makeLens ''Redson

------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
--
-- Note that this works for transparent mode even if model is unknown.
getModelName:: MonadSnap m => m ModelName
getModelName = fromParam "model"


------------------------------------------------------------------------------
-- | Extract model instance id from request parameter.
getInstanceId:: MonadSnap m => m CRUD.InstanceId
getInstanceId = fromParam "id"


------------------------------------------------------------------------------
-- | Extract model instance Redis key from request parameters.
getInstanceKey :: MonadSnap m => m (ModelName, CRUD.InstanceId)
getInstanceKey = (,) <$> getModelName <*> getInstanceId


------------------------------------------------------------------------------
-- | Try to get Model for current request.
--
-- TODO: Return special model for transparent-mode.
getModel :: (MonadSnap m, MonadState (Redson b) m) => m (Maybe Model)
getModel = M.lookup <$> getModelName <*> gets models

getModels :: (MonadSnap m, MonadState (Redson b) m) => m (M.Map ModelName Model)
getModels = gets models


------------------------------------------------------------------------------
-- | Try to get Model with specified name.
getModelNamed :: (MonadSnap m, MonadState (Redson b) m) => ModelName -> m (Maybe Model)
getModelNamed mn = M.lookup mn <$> gets models

