{-|
  
  Model definitions loader.

-}

module Snap.Snaplet.Redson.Snapless.Metamodel.Loader
    ( -- * Lower-level operations
      loadGroups
    , loadModel
    -- * High-level helper
    , loadModels
    )

where

import Control.Monad

import Data.Aeson as A

import Data.Functor

import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy as LB (readFile)

import qualified Data.Map as M

import System.EasyFile


import Snap.Snaplet.Redson.Snapless.Metamodel


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filename = A.decode <$> LB.readFile filename


-- | Load groups from definitions file.
loadGroups :: FilePath -> IO (Maybe Groups)
loadGroups = parseFile


-- | Load model from specified location, performing group splicing,
-- applications and filling index cache.
loadModel :: FilePath
          -- ^ Path to model definition file
          -> Groups 
          -- ^ Group definitions
          -> IO (Maybe Model)
loadModel modelFile groups
    =  (fmap $ cacheIndices
             . doApplications
             . spliceGroups groups)
    <$> parseFile modelFile


-- | Build metamodel name from its file path.
pathToModelName :: FilePath -> ModelName
pathToModelName filepath = BU.fromString $ takeBaseName filepath


-- | Read all models from directory to a map.
--
-- TODO: Perhaps rely on special directory file which explicitly lists
-- all models.
loadModels :: FilePath -- ^ Models directory
           -> FilePath -- ^ Group definitions file
           -> IO (M.Map ModelName Model)
loadModels directory groupsFile =
      do
        dirEntries <- getDirectoryContents directory
        -- Leave out non-files
        mdlFiles <- filterM doesFileExist
                 (map (directory </>) dirEntries)
        gs <- loadGroups groupsFile
        case gs of
          Just groups -> do
                  mdls <- mapM (\m -> do
                                  mres <- loadModel m groups
                                  return $ case mres of
                                    Just mdl -> mdl
                                    Nothing -> error $ "Could not parse " ++ m
                               ) mdlFiles
                  -- Splice groups & cache indices for served models
                  return $ M.fromList $
                         zip (map pathToModelName mdlFiles) mdls
          Nothing -> error $ "Bad groups file " ++ groupsFile
