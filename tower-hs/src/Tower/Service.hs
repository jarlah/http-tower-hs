{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : Tower.Service
-- Description : Core Service and Middleware abstractions
-- License     : MIT
--
-- The fundamental building blocks for composable middleware stacks.
-- A 'Service' is a function from request to @IO (Either ServiceError response)@,
-- and 'Middleware' wraps a service to add behavior.
--
-- 'Service' forms a 'Category' (sequential composition via '>>>') and an
-- 'Arrow', so you can use arrow combinators and proc-notation to wire
-- services together. All instances are derived via
-- @'Kleisli' ('ExceptT' 'ServiceError' 'IO')@, which 'Service' is isomorphic to.
module Tower.Service
  ( Service(..)
  , Middleware
  , mapService
  , contramapService
  , dimapService
  , composeMiddleware
  ) where

import Control.Arrow (Arrow, ArrowChoice, Kleisli(..))
import Control.Category (Category, (.))
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Profunctor (Profunctor(..))
import GHC.IO (IO(..)) -- bring IO's newtype constructor into scope so DerivingVia can coerce through it
import Prelude hiding (id, (.))
import Tower.Error (ServiceError)

-- | A service transforms a request into an effectful response.
-- This is the fundamental building block — middleware wraps services.
--
-- 'Service' is isomorphic to @'Kleisli' ('ExceptT' 'ServiceError' 'IO')@,
-- and all of its instances ('Functor', 'Profunctor', 'Category', 'Arrow',
-- 'ArrowChoice') are derived from that representation. Composition via
-- '>>>' short-circuits on the first 'Left'.
--
-- Use 'fmap' to transform responses, 'lmap' to transform requests, or
-- 'dimap' to transform both — useful for lifting a
-- @Service Http.Request Http.Response@ into a
-- @Service MyBinding.SomeRequest MyBinding.SomeResponse@.
--
-- @
-- let echoService = 'Service' $ \\req -> pure (Right req)
-- result <- 'runService' echoService "hello"
-- -- result == Right "hello"
-- @
newtype Service req res = Service
  { runService :: req -> IO (Either ServiceError res)
    -- ^ Execute the service with a request, returning either an error or a response.
  }
  deriving (Functor) via (Kleisli (ExceptT ServiceError IO) req)
  deriving (Profunctor, Category, Arrow, ArrowChoice)
    via (Kleisli (ExceptT ServiceError IO))

-- | Middleware wraps a service to add behavior (retry, timeout, logging, etc.)
--
-- A middleware is simply a function from 'Service' to 'Service':
--
-- @
-- type Middleware req res = Service req res -> Service req res
-- @
type Middleware req res = Service req res -> Service req res

-- | Transform the response of a service, leaving errors unchanged.
--
-- This is a synonym for 'fmap' on the 'Functor' instance.
--
-- @
-- let svc = 'Service' $ \\_ -> pure (Right 10)
-- let doubled = 'mapService' (* 2) svc
-- result <- 'runService' doubled ()
-- -- result == Right 20
-- @
mapService :: (a -> b) -> Service req a -> Service req b
mapService = fmap

-- | Transform the request type of a service.
--
-- This is a synonym for 'lmap' on the 'Profunctor' instance.
--
-- @
-- let svc :: Service String String
--     svc = 'Service' $ \\req -> pure (Right req)
--     adapted = 'contramapService' show svc  -- now Service Int String
-- result <- 'runService' adapted 42
-- -- result == Right \"42\"
-- @
contramapService :: (a -> b) -> Service b res -> Service a res
contramapService = lmap

-- | Transform both request and response types of a service.
--
-- This is a synonym for 'dimap' on the 'Profunctor' instance.
-- Useful for adapting a generic service to a domain-specific API:
--
-- @
-- let httpSvc :: Service Http.Request Http.Response
-- let apiSvc = 'dimapService' toHttpRequest fromHttpResponse httpSvc
-- -- apiSvc :: Service MyRequest MyResponse
-- @
dimapService :: (a -> b) -> (c -> d) -> Service b c -> Service a d
dimapService = dimap

-- | Compose two middleware, applying the outer first, then the inner.
--
-- @'composeMiddleware' outer inner = outer . inner@
composeMiddleware :: Middleware req res -> Middleware req res -> Middleware req res
composeMiddleware outer inner = outer . inner
