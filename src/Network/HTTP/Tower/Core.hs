module Network.HTTP.Tower.Core
  ( Service(..)
  , Middleware
  , mapService
  , composeMiddleware
  ) where

import Network.HTTP.Tower.Error (ServiceError)

-- | A service transforms a request into an effectful response.
-- This is the fundamental building block — middleware wraps services.
newtype Service req res = Service
  { runService :: req -> IO (Either ServiceError res)
  }

-- | Middleware wraps a service to add behavior (retry, timeout, logging, etc.)
type Middleware req res = Service req res -> Service req res

-- | Transform the response of a service.
mapService :: (a -> b) -> Service req a -> Service req b
mapService f (Service run) = Service $ \req -> fmap (fmap f) (run req)

-- | Compose two middleware: apply outer first, then inner.
-- @composeMiddleware outer inner = outer . inner@
composeMiddleware :: Middleware req res -> Middleware req res -> Middleware req res
composeMiddleware outer inner = outer . inner
