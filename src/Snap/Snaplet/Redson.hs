{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

{-|

CRUD for JSON data with Redis storage.

Can be used as Backbone.sync backend.

-}

module Snap.Snaplet.Redson (Redson
                           , redsonInit)
where

import Prelude hiding (concat, FilePath, id)

import Control.Monad.State hiding (put)
import Data.Functor

import Data.Aeson as A

import Data.Char (isDigit)
import Numeric (readDec)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB (ByteString, readFile)
import qualified Data.ByteString.UTF8 as BU (fromString, toString)

import Data.Configurator

import Data.Lens.Common
import Data.Lens.Template

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

import System.EasyFile

import Snap.Snaplet.Redson.CRUD
import Snap.Snaplet.Redson.Metamodel
import Snap.Snaplet.Redson.Util

------------------------------------------------------------------------------
-- | Redson snaplet state type.
--
-- *TODO*: Use HashMap to store models?
data Redson b = Redson
             { _database :: Snaplet RedisDB
             , auth :: Lens b (Snaplet (AuthManager b))
             , events :: PS.PubSub Hybi10
             , models :: M.Map ModelName Model
             , transparent :: Bool
             -- ^ Operate in transparent mode (not security checks).
             }

makeLens ''Redson


------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
getModelName:: MonadSnap m => m ModelName
getModelName = fromParam "model"


------------------------------------------------------------------------------
-- | Extract model instance id from request parameter.
getModelId:: MonadSnap m => m InstanceId
getModelId = fromParam "id"



------------------------------------------------------------------------------
-- | Extract model instance Redis key from request parameters.
getInstanceKey :: MonadSnap m => m B.ByteString
getInstanceKey = liftM2 instanceKey getModelName getModelId

------------------------------------------------------------------------------
-- | Try to get Model for current request.
--
-- TODO: Return special model for transparent-mode.
getModel :: (MonadSnap m, MonadState (Redson b) m) => m (Maybe Model)
getModel = liftM2 M.lookup getModelName (gets models)


------------------------------------------------------------------------------
-- | Perform action with AuthManager.
withAuth :: (MonadState (Redson b1) (m b1 v), MonadSnaplet m) =>
            m b1 (AuthManager b1) b -> m b1 v b
withAuth action = do
  am <- gets auth
  return =<< withTop am action


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
                 -> InstanceId
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
                -> InstanceId
                -> Network.WebSockets.Message p
creationMessage = modelMessage "create"

-- | Model instance deletion message.
deletionMessage :: ModelName
                -> InstanceId
                -> Network.WebSockets.Message p
deletionMessage = modelMessage "delete"


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to B.ByteString with JSON.
commitToJson :: Commit -> LB.ByteString
commitToJson r = A.encode r


------------------------------------------------------------------------------
-- | Decode B.ByteString with JSON to map of hash keys & values for
-- Redis HMSET (still to be `toList`-ed).
--
-- Return Nothing if parsing failed.
--
-- Note that if JSON object contains `null` values, conversion will
-- fail.
jsonToCommit :: LB.ByteString -> Maybe Commit
jsonToCommit s =
    let
        j = A.decode s
    in
      case j of
        Nothing -> Nothing
        Just m ->
             -- Omit fields with null values and "id" key
            Just (M.filterWithKey 
                       (\k _ -> k /= "id")
                       m)


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
        Right newId <- runRedisDB database $
           create mname commit (maybe [] indices mdl)

        ps <- gets events
        liftIO $ PS.publish ps $ creationMessage mname newId

        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5:
        --
        -- the response SHOULD be 201 (Created) and contain an entity which
        -- describes the status of the request and refers to the new
        -- resource
        modifyResponse $ (setContentType "application/json" . setResponseCode 201)
        -- Tell client new instance id in response JSON.
        writeLBS $ A.encode $ M.insert "id" newId commit
        return ()


------------------------------------------------------------------------------
-- | Read instance from Redis.
read' :: Handler b (Redson b) ()
read' = ifTop $ do
  -- Pass to index page handler (Snap routing bug workaround)
  id <- fromParam "id"
  when (B.null id)
       pass

  withCheckSecurity $ \au mdl -> do
    key <- getInstanceKey
    r <- runRedisDB database $ do
      Right r <- hgetall key
      return r

    when (null r) $
         handleError notFound

    modifyResponse $ setContentType "application/json"
    writeLBS $ commitToJson $ (filterUnreadable au mdl (M.fromList r))
    return ()


------------------------------------------------------------------------------
-- | Handle PUT request for existing instance in Redis.
--
-- *TODO* Report 201 if could create new instance.
put :: Handler b (Redson b) ()
put = ifTop $ do
  withCheckSecurity $ \au mdl -> do
    -- Parse request body to list of pairs
    r <- jsonToCommit <$> getRequestBody
    case r of
      Nothing -> handleError serverError
      Just j -> do
        when (not $ checkWrite au mdl j) $
             handleError forbidden

        id <- getModelId
        mname <- getModelName        
        resp <- runRedisDB database $ 
           update mname id j (maybe [] indices mdl)
        case resp of
          Left err -> handleError err
          Right _ -> modifyResponse $ setResponseCode 204
        return ()


------------------------------------------------------------------------------
-- | Delete instance from Redis (including timeline).
delete :: Handler b (Redson b) ()
delete = ifTop $ do
  withCheckSecurity $ \_ _ -> do
    id <- getModelId
    mname <- getModelName
    key <- getInstanceKey

    r <- runRedisDB database $ do
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7
      --
      -- A successful response SHOULD be 200 (OK) if the response includes
      -- an entity describing the status
      Right r <- hgetall key
      return r

    when (null r) $
         handleError notFound

    runRedisDB database $ lrem (modelTimeline mname) 1 id >> del [key]

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
      Right r <- lrange (modelTimeline mname) 0 9
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
            gets (filter (\(_, m) -> elem GET $
                                     getModelPermissions (Right user) m)
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
-- TODO Allow to request only subset of fields and serve them in array.
search :: Handler b (Redson b) ()
search = 
    let
        intersectAll = foldl1' intersect
        unionAll = foldl1' union
        -- Get list of ids which match single search term
        getTermIds pattern = runRedisDB database $ do
          Right sets <- keys pattern
          case sets of
            [] -> return []
            _ -> do
              -- Hedis hangs when doing `suinion []`
              -- 
              -- TODO Use sunionstore and perform further operations
              -- on Redis as well.
              Right ids <- sunion sets
              return ids
        -- Fetch instance by id to JSON
        fetchInstance id key = runRedisDB database $ do
          Right r <- hgetall key
          return $ (M.fromList $ ("id", id):r)
    in
     ifTop $
       withCheckSecurity $ \_ mdl -> do
         case mdl of
           Nothing -> handleError notFound
           Just m -> do
               mname <- getModelName
               -- TODO: Mark these field names as reserved
               mType <- getParam "_matchType"
               sType <- getParam "_searchType"
               iLimit <- getParam "_limit"

               patFunction <- return $ case mType of
                               Just "p"  -> prefixMatch
                               Just "s"  -> substringMatch
                               _         -> prefixMatch

               searchType  <- return $ case sType of
                               Just "and" -> intersectAll
                               Just "or"  -> unionAll
                               _          -> intersectAll

               itemLimit   <- return $ case iLimit of
                               Just b -> let 
                                            s = BU.toString b
                                         in
                                           if (all isDigit s) then (read s)
                                           else defaultSearchLimit
                               _      -> defaultSearchLimit

               -- Try to get search results for every index field
               termIds <- mapM (\i -> do
                                  p <- getParam i
                                  case p of
                                    Nothing -> return Nothing
                                    Just s -> do
                                      ids <- getTermIds (patFunction mname i s)
                                      return $ Just ids)
                          (indices m)
               modifyResponse $ setContentType "application/json"
               case (catMaybes termIds) of
                 [] -> writeLBS $ A.encode ([] :: [Value])
                 tids -> do
                       -- Finally, list of matched instances
                       instances <- mapM (\id -> fetchInstance id $
                                                 instanceKey mname id)
                                    (searchType tids)
                       writeLBS $ A.encode (take itemLimit instances)
         return ()

-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: [(B.ByteString, Handler b (Redson b) ())]
routes = [ (":model/timeline", method GET timeline)
         , (":model/events", modelEvents)
         , (":model/model", method GET metamodel)
         , ("_models", method GET listModels)
         , (":model", method POST post)
         , (":model/:id", method GET read')
         , (":model/:id", method PUT put)
         , (":model/:id", method DELETE delete)
         , (":model/search/", method GET search)
         ]


-- | Build metamodel name from its file path.
pathToModelName :: FilePath -> ModelName
pathToModelName filepath = BU.fromString $ takeBaseName filepath


-- | Read all models from directory to a map.
--
-- TODO: Perhaps rely on special directory file which explicitly lists
-- all models.
loadModels :: FilePath -> IO (M.Map ModelName Model)
loadModels directory =
    let
        parseModel :: FilePath -> IO Model
        parseModel filename = do
              j <- LB.readFile filename
              case (A.decode j) of
                Just model -> return model
                Nothing -> error $ "Could not parse " ++ filename
    in
      do
        dirEntries <- getDirectoryContents directory
        -- Leave out non-files
        files <- filterM doesFileExist (map (\f -> directory ++ "/" ++ f) dirEntries)
        mdls <- mapM parseModel files
        return $ M.fromList $ zip (map pathToModelName files) mdls


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redsonInit :: Lens b (Snaplet (AuthManager b))
           -> SnapletInit b (Redson b)
redsonInit topAuth = makeSnaplet
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

            mdls <- liftIO $ loadModels mdlDir
            addRoutes routes
            return $ Redson r topAuth p mdls transp
