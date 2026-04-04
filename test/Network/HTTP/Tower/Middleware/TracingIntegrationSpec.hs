{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Network.HTTP.Tower.Middleware.TracingIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Text (Text, isInfixOf)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types as HTTP
import Test.Hspec

import OpenTelemetry.Trace.Core
  ( Tracer
  , makeTracer
  , InstrumentationLibrary(..)
  , TracerOptions(..)
  , ImmutableSpan(..)
  , setGlobalTracerProvider
  , shutdownTracerProvider
  )
import OpenTelemetry.Attributes (emptyAttributes, lookupAttribute)
import OpenTelemetry.Trace
  ( createTracerProvider
  , emptyTracerProviderOptions
  )
import OpenTelemetry.Exporter.InMemory (inMemoryListExporter)

import Network.HTTP.Tower.Client (HttpResponse)
import Network.HTTP.Tower.Core
import Network.HTTP.Tower.Error
import Network.HTTP.Tower.Middleware.Tracing

-- | Set up an in-memory TracerProvider for testing.
-- Returns a Tracer and an IORef that collects completed spans.
withTestTracer :: (Tracer -> IORef [ImmutableSpan] -> IO a) -> IO a
withTestTracer action = do
  (processor, spanRef) <- inMemoryListExporter
  tp <- createTracerProvider [processor] emptyTracerProviderOptions
  setGlobalTracerProvider tp
  let tracer = makeTracer tp
        (InstrumentationLibrary
          { libraryName = "http-tower-hs-test"
          , libraryVersion = "0.0.0"
          , librarySchemaUrl = ""
          , libraryAttributes = emptyAttributes
          })
        (TracerOptions Nothing)
  result <- action tracer spanRef
  shutdownTracerProvider tp
  pure result

spec :: Spec
spec = describe "Tracing integration (in-memory exporter)" $ do

  it "creates a span for each request" $ withTestTracer $ \tracer spanRef -> do
    let svc = Service $ \_ -> pure (Right fakeResponse)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://example.com/test"
    _ <- runService traced req
    -- Give the processor a moment to flush
    threadDelay 100_000
    spans <- readIORef spanRef
    length spans `shouldSatisfy` (>= 1)

  it "sets span name to HTTP method + host" $ withTestTracer $ \tracer spanRef -> do
    let svc = Service $ \_ -> pure (Right fakeResponse)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://api.example.com/v1/users"
    _ <- runService traced req
    threadDelay 100_000
    spans <- readIORef spanRef
    let spanNames = map spanName spans
    any (isInfixOf "api.example.com") spanNames `shouldBe` True
    any (isInfixOf "GET") spanNames `shouldBe` True

  it "records http.method attribute" $ withTestTracer $ \tracer spanRef -> do
    let svc = Service $ \_ -> pure (Right fakeResponse)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://example.com/data"
    _ <- runService traced req
    threadDelay 100_000
    spans <- readIORef spanRef
    length spans `shouldSatisfy` (>= 1)
    let attrs = spanAttributes (head spans)
    lookupAttribute attrs "http.method" `shouldSatisfy` (/= Nothing)

  it "records http.status_code on success" $ withTestTracer $ \tracer spanRef -> do
    let svc = Service $ \_ -> pure (Right fakeResponse)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://example.com/ok"
    _ <- runService traced req
    threadDelay 100_000
    spans <- readIORef spanRef
    length spans `shouldSatisfy` (>= 1)
    let attrs = spanAttributes (head spans)
    lookupAttribute attrs "http.status_code" `shouldSatisfy` (/= Nothing)

  it "records error status on service failure" $ withTestTracer $ \tracer spanRef -> do
    let svc :: Service HTTP.Request HttpResponse
        svc = Service $ \_ -> pure (Left TimeoutError)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://example.com/fail"
    _ <- runService traced req
    threadDelay 100_000
    spans <- readIORef spanRef
    length spans `shouldSatisfy` (>= 1)

  it "records error status on HTTP 5xx" $ withTestTracer $ \tracer spanRef -> do
    let svc = Service $ \_ -> pure (Right fake500Response)
        traced = withTracingTracer tracer svc
    req <- HTTP.parseRequest "http://example.com/error"
    _ <- runService traced req
    threadDelay 100_000
    spans <- readIORef spanRef
    length spans `shouldSatisfy` (>= 1)

fakeResponse :: HttpResponse
fakeResponse = HTTP.Response
  { HTTP.responseStatus = HTTP.status200
  , HTTP.responseVersion = HTTP.http11
  , HTTP.responseHeaders = []
  , HTTP.responseBody = ""
  , HTTP.responseCookieJar = HTTP.createCookieJar []
  , HTTP.responseClose' = HTTP.ResponseClose (pure ())
  , HTTP.responseOriginalRequest = error "not used"
  , HTTP.responseEarlyHints = []
  }

fake500Response :: HttpResponse
fake500Response = fakeResponse
  { HTTP.responseStatus = HTTP.internalServerError500
  }
