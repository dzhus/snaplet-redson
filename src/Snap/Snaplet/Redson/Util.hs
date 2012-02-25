{-# LANGUAGE OverloadedStrings #-}

{-|

Miscellaneous functions for Snap.

-}

module Snap.Snaplet.Redson.Util where

import Control.Applicative

import Data.ByteString (ByteString)
import Data.Maybe

import Snap.Core

------------------------------------------------------------------------------
-- | Get parameter value from Request or return empty string
fromParam :: MonadSnap m => ByteString -> m ByteString
fromParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with 404 Not found
notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseCode 404
  r <- getResponse
  finishWith r


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with 500 Server error
serverError :: MonadSnap m => m ()
serverError = do
  modifyResponse $ setResponseCode 500
  r <- getResponse
  finishWith r


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with 401 Unauthorized
unauthorized :: MonadSnap m => m ()
unauthorized = do
  modifyResponse $ setResponseCode 401
  r <- getResponse
  finishWith r
