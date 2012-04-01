{-# LANGUAGE OverloadedStrings #-}

{-|

Miscellaneous functions for Snap.

-}

module Snap.Snaplet.Redson.Util where

import Control.Applicative

import Data.Char (isDigit)

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU (toString)

import Data.Maybe

import Snap.Core

------------------------------------------------------------------------------
-- | Get parameter value from Request or return empty string
fromParam :: MonadSnap m => ByteString -> m ByteString
fromParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Get integer parameter value from Request or return default value.
fromIntParam :: MonadSnap m => ByteString -> Int -> m Int
fromIntParam p def = do
  i <- getParam p
  return $ case i of
      Just b -> let
          s = BU.toString b
          in
            if (all isDigit s) then (read s)
            else def
      _ -> def


data Error = Error { code :: Int
                   -- ^ Error response code
                   }


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with error response
handleError :: MonadSnap m => Error -> m ()
handleError err = do
    modifyResponse $ setResponseCode (code err)
    r <- getResponse
    finishWith r


notFound :: Error
notFound = Error 404

serverError :: Error
serverError = Error 500

unauthorized :: Error
unauthorized = Error 401

forbidden :: Error
forbidden = Error 403
