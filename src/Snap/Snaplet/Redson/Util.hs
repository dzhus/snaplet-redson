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
