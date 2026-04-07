-- |
-- Module      : Network.HTTP.Tower.Middleware.TestDouble
-- Description : HTTP-specific mock services and request recording for testing
-- License     : MIT
--
-- @
-- import Data.Function (('&'))
-- -- Replace the service entirely
-- let testSvc = svc '&' 'withMock' (\\req -> pure (Right fakeResponse))
--
-- -- Route-based mocks
-- let testSvc = svc '&' 'withMockMap' mocks
--
-- -- Record requests for assertions
-- recorder <- newIORef []
-- let testSvc = svc '&' 'withRecorder' recorder
-- @
module Network.HTTP.Tower.Middleware.TestDouble
  ( withMock
  , withMockMap
  , withRecorder
  ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as HTTP

import Network.HTTP.Tower.Client (HttpResponse)
import Tower.Service (Service(..), Middleware)
import Tower.Error (ServiceError(..))

-- | Replace the inner service entirely with a mock function.
-- The inner service is never called.
withMock
  :: (HTTP.Request -> IO (Either ServiceError HttpResponse))
  -> Middleware HTTP.Request HttpResponse
withMock handler _inner = Service handler

-- | Route requests to different mock responses based on @host <> path@.
-- Falls through to the inner service if no match is found.
--
-- @
-- import Data.Function (('&'))
-- let mocks = Map.fromList
--       [ (\"api.example.com\/v1\/users\", Right usersResponse)
--       , (\"api.example.com\/v1\/health\", Right healthResponse)
--       ]
-- let testSvc = svc '&' 'withMockMap' mocks
-- @
withMockMap
  :: Map ByteString (Either ServiceError HttpResponse)
  -> Middleware HTTP.Request HttpResponse
withMockMap routes inner = Service $ \req ->
  let key = HTTP.host req <> HTTP.path req
  in case Map.lookup key routes of
    Just result -> pure result
    Nothing     -> runService inner req

-- | Record all requests that pass through, then forward to the inner service.
-- The recorder stores requests in reverse order (most recent first).
--
-- @
-- import Data.Function (('&'))
-- recorder <- newIORef []
-- let testSvc = svc '&' 'withRecorder' recorder
-- _ <- runService testSvc someRequest
-- recorded <- readIORef recorder
-- length recorded \`shouldBe\` 1
-- @
withRecorder :: IORef [HTTP.Request] -> Middleware HTTP.Request HttpResponse
withRecorder ref inner = Service $ \req -> do
  modifyIORef' ref (req :)
  runService inner req
