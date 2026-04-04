{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Network.HTTP.Tower.Middleware.TracingDockerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, SomeException)
import Data.Aeson (Value(..), (.:), decode, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP
import System.Environment (setEnv, unsetEnv)
import System.Process (callCommand, readProcess)
import Test.Hspec

import OpenTelemetry.Trace.Core
  ( makeTracer
  , InstrumentationLibrary(..)
  , TracerOptions(..)
  , shutdownTracerProvider
  )
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Trace
  ( initializeGlobalTracerProvider
  )

import Network.HTTP.Tower.Client (HttpResponse)
import Network.HTTP.Tower.Core
import Network.HTTP.Tower.Middleware.Tracing

-- | Check if Docker is available.
dockerAvailable :: IO Bool
dockerAvailable = do
  result <- try (readProcess "docker" ["info"] "") :: IO (Either SomeException String)
  pure $ case result of
    Right _ -> True
    Left _  -> False

-- | Start Jaeger all-in-one container, run action, then stop it.
withJaeger :: IO a -> IO a
withJaeger action = bracket startJaeger stopJaeger (const action)
  where
    startJaeger = do
      _ <- try (callCommand "docker rm -f http-tower-jaeger 2>/dev/null") :: IO (Either SomeException ())
      callCommand $ unwords
        [ "docker run -d --name http-tower-jaeger"
        , "-p 4318:4318"    -- OTLP HTTP receiver
        , "-p 16686:16686"  -- Jaeger UI / API
        , "jaegertracing/all-in-one:latest"
        ]
      waitForJaeger 30
    stopJaeger _ = do
      _ <- try (callCommand "docker rm -f http-tower-jaeger") :: IO (Either SomeException ())
      pure ()

-- | Poll Jaeger until ready.
waitForJaeger :: Int -> IO ()
waitForJaeger 0 = fail "Jaeger did not become ready in time"
waitForJaeger retries = do
  mgr <- HTTP.newManager TLS.tlsManagerSettings
  req <- HTTP.parseRequest "http://localhost:16686/"
  result <- try (HTTP.httpLbs req mgr) :: IO (Either SomeException (HTTP.Response LBS.ByteString))
  case result of
    Right _ -> pure ()
    Left _ -> do
      threadDelay 1_000_000
      waitForJaeger (retries - 1)

-- | Query Jaeger API for traces from a given service.
queryJaegerTraces :: HTTP.Manager -> String -> IO (Maybe Value)
queryJaegerTraces mgr service = do
  req <- HTTP.parseRequest $
    "http://localhost:16686/api/traces?service=" <> service <> "&limit=10"
  resp <- HTTP.httpLbs req mgr
  pure (decode (HTTP.responseBody resp))

spec :: Spec
spec = describe "Tracing Docker integration (Jaeger)" $ beforeAll dockerAvailable $ do

  it "exports spans to Jaeger via OTLP" $ \isAvailable -> do
    if not isAvailable
      then pendingWith "Docker not available, skipping Jaeger integration test"
      else withJaeger $ do
        -- Configure OTel SDK via environment variables
        setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" "http://localhost:4318"
        setEnv "OTEL_EXPORTER_OTLP_PROTOCOL" "http/protobuf"
        setEnv "OTEL_SERVICE_NAME" "http-tower-hs-docker-test"

        -- Initialize global tracer provider (reads env vars)
        tp <- initializeGlobalTracerProvider

        let tracer = makeTracer tp
              (InstrumentationLibrary
                { libraryName = "http-tower-hs-docker-test"
                , libraryVersion = "0.1.0.0"
                , librarySchemaUrl = ""
                , libraryAttributes = emptyAttributes
                })
              (TracerOptions Nothing)

        -- Run a request through the tracing middleware
        let svc = Service $ \_ -> pure (Right fakeResponse)
            traced = withTracingTracer tracer svc
        req <- HTTP.parseRequest "http://example.com/docker-test"
        _ <- runService traced req

        -- Shut down to flush all spans
        shutdownTracerProvider tp

        -- Clean up env vars
        unsetEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
        unsetEnv "OTEL_EXPORTER_OTLP_PROTOCOL"
        unsetEnv "OTEL_SERVICE_NAME"

        -- Give Jaeger time to index
        threadDelay 3_000_000

        -- Query Jaeger for our traces
        mgr <- HTTP.newManager TLS.tlsManagerSettings
        result <- queryJaegerTraces mgr "http-tower-hs-docker-test"
        case result of
          Nothing -> expectationFailure "Failed to parse Jaeger API response"
          Just val -> do
            let traceCount = parseMaybe (withObject "resp" $ \o -> do
                    arr <- o .: "data"
                    pure (V.length (arr :: V.Vector Value))) val
            case traceCount of
              Just n  -> n `shouldSatisfy` (>= 1)
              Nothing -> expectationFailure $ "Unexpected Jaeger response: " ++ show val

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
