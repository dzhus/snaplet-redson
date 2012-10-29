{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

CRUD for JSON data with Redis storage.

Can be used as Backbone.sync backend.

-}

module Snap.Snaplet.Redson
    ( Redson
    , redsonInit
    , redsonInitWithHooks
    )

where

import qualified Prelude (id)
import Prelude hiding (concat, FilePath, id, read)

import Control.Applicative
import Control.Monad.State hiding (put)

import Data.Aeson as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB (ByteString)

import Data.Configurator

import Data.Lens.Common

import Data.List (foldl1', intersect, union)
import qualified Data.Map as M

import Data.Maybe

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.RedisDB

import Network.WebSockets
import Network.WebSockets.Snap
import qualified Network.WebSockets.Util.PubSub as PS

import Database.Redis hiding (auth)


import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD
import Snap.Snaplet.Redson.Snapless.Metamodel
import Snap.Snaplet.Redson.Snapless.Metamodel.Loader (loadModels)
import Snap.Snaplet.Redson.Permissions
import Snap.Snaplet.Redson.Search
import Snap.Snaplet.Redson.Util
import Snap.Snaplet.Redson.Internals



------------------------------------------------------------------------------
-- | Perform action with AuthManager.
withAuth :: (MonadState (Redson b1) (m b1 v), MonadSnaplet m) =>
            m b1 (AuthManager b1) b -> m b1 v b
withAuth = (gets auth >>=) . flip withTop


------------------------------------------------------------------------------
-- | Top-level (per-form) security checking.
--
-- Reject request if no user is logged in or metamodel is unknown or
-- user has no permissions for CRUD method; otherwise perform given
-- handler action with user and metamodel as arguments. In transparent
-- mode, always perform the action without any checks.
--
-- If security checks are in effect and succeed, action is always
-- called with Just constructor of Maybe Model.
withCheckSecurity :: (Either SuperUser AuthUser -> Maybe Model
                  -> Handler b (Redson b) ())
                  -> Handler b (Redson b) ()
withCheckSecurity action = do
  mdl <- getModel
  trs <- gets transparent
  case trs of
    True -> action (Left SuperUser) mdl
    False -> do
      m <- getsRequest rqMethod
      au <- withAuth currentUser
      case (au, mdl) of
        (Nothing, _) -> handleError unauthorized
        (_, Nothing) -> handleError forbidden
        (Just user, Just model) ->
           case (elem m $ getModelPermissions (Right user) model) of
             True -> action (Right user) mdl
             False -> handleError forbidden


------------------------------------------------------------------------------
-- | Builder for WebSockets message containing JSON describing
-- creation or deletion of model instance.
modelMessage :: B.ByteString
             -> (ModelName
                 -> CRUD.InstanceId
                 -> Network.WebSockets.Message p)
modelMessage event = \model id ->
    let
        response :: [(B.ByteString, B.ByteString)]
        response = [("event", event),
                    ("id", id),
                    ("model", model)]
    in
      DataMessage $ Text $ A.encode $ M.fromList response


-- | Model instance creation message.
creationMessage :: ModelName
                -> CRUD.InstanceId
                -> Network.WebSockets.Message p
creationMessage = modelMessage "create"


-- | Model instance deletion message.
deletionMessage :: ModelName
                -> CRUD.InstanceId
                -> Network.WebSockets.Message p
deletionMessage = modelMessage "delete"


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to B.ByteString with JSON.
commitToJson :: Commit -> LB.ByteString
commitToJson = A.encode


------------------------------------------------------------------------------
applyHooks :: ModelName -> Commit -> Handler b (Redson b) Commit
applyHooks mname commit = do
  hs <- gets hookMap
  case M.lookup mname hs of
    Nothing -> return commit
    Just h  ->
      let actions = M.intersectionWith (map . flip ($)) commit h
      in  apply' commit actions
  where
    apply' c = foldM apply'' c . M.elems
    apply''  = foldM (flip ($))


------------------------------------------------------------------------------
-- | Handle instance creation request
--
-- *TODO*: Use readRequestBody
post :: Handler b (Redson b) ()
post = ifTop $ do
  withCheckSecurity $ \au mdl -> do
    -- Parse request body to list of pairs
    r <- jsonToCommit <$> getRequestBody
    case r of
      Nothing -> handleError serverError
      Just commit -> do
        when (not $ checkWrite au mdl commit) $
             handleError forbidden

        mname <- getModelName
        let commit' = maybe commit (M.union commit . defaults) mdl 
        commit'' <- applyHooks mname commit'

        Right newId <- runRedisDB database $
           CRUD.create mname commit'' (maybe [] indices mdl)

        ps <- gets events
        liftIO $ PS.publish ps $ creationMessage mname newId

        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5:
        --
        -- the response SHOULD be 201 (Created) and contain an entity which
        -- describes the status of the request and refers to the new
        -- resource
        modifyResponse $ setContentType "application/json" . setResponseCode 201
        -- Tell client new instance id in response JSON.
        writeLBS $ A.encode $ M.insert "id" newId commit''


------------------------------------------------------------------------------
-- | Read instance from Redis.
get' :: Handler b (Redson b) ()
get' = ifTop $ do
  withCheckSecurity $ \au mdl -> do
    (mname, id) <- getInstanceKey

    Right r <- runRedisDB database $ CRUD.read mname id

    when (M.null r) $
         handleError notFound

    modifyResponse $ setContentType "application/json"
    writeLBS $ commitToJson $ filterUnreadable au mdl r


------------------------------------------------------------------------------
-- | Handle PUT request for existing instance in Redis.
--
-- TODO Report 201 if could create new instance.
-- (http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.6)
put :: Handler b (Redson b) ()
put = ifTop $ do
  withCheckSecurity $ \au mdl -> do
    -- Parse request body to list of pairs
    r <- jsonToCommit <$> getRequestBody
    case r of
      Nothing -> handleError serverError
      Just commit -> do
        when (not $ checkWrite au mdl commit) $
             handleError forbidden

        id <- getInstanceId
        mname <- getModelName
        commit' <- applyHooks mname commit
        Right _ <- runRedisDB database $
           CRUD.update mname id commit' (maybe [] indices mdl)
        modifyResponse $ setContentType "application/json"
        writeLBS $ A.encode $ M.differenceWith
          (\a b -> if a == b then Nothing else Just a)
          commit' commit


------------------------------------------------------------------------------
-- | Delete instance from Redis (including timeline).
delete :: Handler b (Redson b) ()
delete = ifTop $ do
  withCheckSecurity $ \_ mdl -> do
    mname <- getModelName
    id <- getInstanceId

    let key = CRUD.instanceKey mname id

    r <- runRedisDB database $ do
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7
      --
      -- A successful response SHOULD be 200 (OK) if the response includes
      -- an entity describing the status
      Right r <- hgetall key
      return r

    when (null r) $
         handleError notFound

    runRedisDB database $ CRUD.delete mname id (maybe [] indices mdl)

    modifyResponse $ setContentType "application/json"
    writeLBS (commitToJson (M.fromList r))

    ps <- gets events
    liftIO $ PS.publish ps $ deletionMessage mname id


------------------------------------------------------------------------------
-- | Serve list of 10 latest instances stored in Redis.
--
-- *TODO*: Adjustable item limit.
timeline :: Handler b (Redson b) ()
timeline = ifTop $ do
  withCheckSecurity $ \_ _ -> do
    mname <- getModelName

    r <- runRedisDB database $ do
      Right r <- lrange (CRUD.modelTimeline mname) 0 9
      return r

    modifyResponse $ setContentType "application/json"
    writeLBS (enc' r)
      where
          enc' :: [B.ByteString] -> LB.ByteString
          enc' r = A.encode r


------------------------------------------------------------------------------
-- | WebSockets handler which pushes instance creation/deletion events
-- to client.
--
-- TODO: Check for login?
modelEvents :: Handler b (Redson b) ()
modelEvents = ifTop $ do
  ps <- gets events
  liftSnap $ runWebSocketsSnap (\r -> do
                                  acceptRequest r
                                  PS.subscribe ps)


------------------------------------------------------------------------------
-- | Serve JSON metamodel with respect to current user and field
-- permissions.
--
-- TODO: Cache this wrt user permissions cache.
metamodel :: Handler b (Redson b) ()
metamodel = ifTop $ do
  withCheckSecurity $ \au mdl -> do
    case mdl of
      Nothing -> handleError notFound
      Just m -> do
        modifyResponse $ setContentType "application/json"
        writeLBS (A.encode $ stripModel au m)


------------------------------------------------------------------------------
-- | Serve JSON array of readable models to user. Every array element
-- is an object with fields "name" and "title". In transparent mode,
-- serve all models.
--
-- TODO: Cache this.
listModels :: Handler b (Redson b) ()
listModels = ifTop $ do
  au <- withAuth currentUser
  trs <- gets transparent
  readables <- case trs of
    True -> gets (M.toList . models)
    False ->
      case au of
        -- Won't get to serving [] anyways.
        Nothing -> handleError unauthorized >> return []
        -- Leave only readable models.
        Just user ->
            gets (filter (elem GET
                          . getModelPermissions (Right user) . snd)
                  . M.toList . models)
  modifyResponse $ setContentType "application/json"
  writeLBS (A.encode $
             map (\(n, m) -> M.fromList $
                             [("name"::B.ByteString, n),
                              ("title", title m)])
             readables)


defaultSearchLimit :: Int
defaultSearchLimit = 100


-----------------------------------------------------------------------------
-- | Serve model instances which have index values containing supplied
-- search parameters.
--
-- Currently not available in transparent mode.
search :: Handler b (Redson b) ()
search =
    let
        intersectAll = foldl1' intersect
        unionAll = foldl1' union
        -- Fetch instance by id to JSON
        fetchInstance id key = runRedisDB database $ do
                                 Right r <- hgetall key
                                 return $ (M.fromList $ ("id", id):r)
        comma = 0x2c
    in
      ifTop $ withCheckSecurity $ \_ mdl -> do
        case mdl of
          Nothing -> handleError notFound
          Just m ->
            let
                mname = modelName m
            in do
              -- TODO: Mark these field names as reserved
              mType <- getParam "_matchType"
              sType <- getParam "_searchType"
              outFields <- maybe [] (B.split comma) <$>
                           getParam "_fields"

              let patFunction = case mType of
                               Just "p"  -> prefixMatch
                               Just "s"  -> substringMatch
                               _         -> prefixMatch

              let searchType  = case sType of
                               Just "and" -> intersectAll
                               Just "or"  -> unionAll
                               _          -> intersectAll

              itemLimit   <- fromIntParam "_limit" defaultSearchLimit

              query       <- fromMaybe "" <$> getParam "q"
              -- Produce Just SearchTerm
              let collate c = if c then CRUD.collate else Prelude.id
              let indexValues = map (mapSnd (`collate` query)) $ indices m

              -- For every term, get list of ids which match it
              termIds <- runRedisDB database $
                         redisSearch m indexValues patFunction

              modifyResponse $ setContentType "application/json"
              case (filter (not . null) termIds) of
                [] -> writeLBS $ A.encode ([] :: [Value])
                tids -> do
                      -- Finally, list of matched instances
                      instances <- take itemLimit <$>
                                   mapM (\id -> fetchInstance id $
                                         CRUD.instanceKey mname id)
                                  (searchType tids)
                      -- If _fields provided, leave only requested
                      -- fields and serve array of arrays. Otherwise,
                      -- serve array of objects.
                      case outFields of
                        [] -> writeLBS $ A.encode instances
                        _ -> writeLBS $ A.encode $
                             map (`CRUD.onlyFields` outFields) instances


mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)


-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: [(B.ByteString, Handler b (Redson b) ())]
routes = [ (":model/timeline", method GET timeline)
         , (":model/events", modelEvents)
         , (":model/model", method GET metamodel)
         , ("_models", method GET listModels)
         , (":model", method POST post)
         , (":model/:id", method GET get')
         , (":model/:id", method PUT put)
         , (":model/:id", method DELETE delete)
         , (":model/search/", method GET search)
         ]


------------------------------------------------------------------------------
-- | Initialize Redson. AuthManager from parent snaplet is required.
--
-- Connect to Redis, read configuration and set routes.
--
-- > appInit :: SnapletInit MyApp MyApp
-- > appInit = makeSnaplet "app" "App with Redson" Nothing $
-- >           do
-- >             r <- nestSnaplet "_" redson $ redsonInit auth
-- >             s <- nestSnaplet "session" session $ initCookieSessionManager
-- >                                                  sesKey "_session" sessionTimeout
-- >             a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings
-- >             return $ MyApp r s a
redsonInit:: Lens b (Snaplet (AuthManager b))
           -> SnapletInit b (Redson b)
redsonInit = (`redsonInitWithHooks` M.empty)

redsonInitWithHooks :: Lens b (Snaplet (AuthManager b))
           -> HookMap b
           -> SnapletInit b (Redson b)
redsonInitWithHooks topAuth hooks = makeSnaplet
                     "redson"
                     "CRUD for JSON data with Redis storage"
                     Nothing $
          do
            r <- nestSnaplet "db" database $ redisDBInit defaultConnectInfo
            p <- liftIO PS.newPubSub

            cfg <- getSnapletUserConfig
            mdlDir <- liftIO $
                      lookupDefault "resources/models/"
                                    cfg "models-directory"

            transp <- liftIO $
                      lookupDefault False
                                    cfg "transparent-mode"

            grpDef <- liftIO $
                      lookupDefault "resources/field-groups.json"
                                    cfg "field-groups-file"

            mdls <- liftIO $ loadModels mdlDir grpDef
            addRoutes routes
            return $ Redson r topAuth p mdls transp hooks
