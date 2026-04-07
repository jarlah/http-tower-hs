{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Tower.Middleware.ValidateSpec (spec) where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types as HTTP
import Test.Hspec

import Network.HTTP.Tower.Client (HttpResponse)
import Tower.Service
import Tower.Error
import Tower.Error.Testing ()
import Data.Function ((&))
import Network.HTTP.Tower.Middleware.Validate

spec :: Spec
spec = describe "Validate middleware" $ do
  describe "withValidateStatus" $ do
    it "passes valid status codes" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200 []))
          validated = svc & withValidateStatus (\c -> c >= 200 && c < 300)
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Right resp -> HTTP.responseStatus resp `shouldBe` HTTP.status200
        Left err -> expectationFailure $ show err

    it "rejects invalid status codes" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.internalServerError500 []))
          validated = svc & withValidateStatus (\c -> c >= 200 && c < 300)
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Left (CustomError msg) -> msg `shouldBe` "Unexpected status code: 500"
        other -> expectationFailure $ "Expected CustomError, got: " ++ show other

  describe "withValidateContentType" $ do
    it "passes matching content type" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200
              [("Content-Type", "application/json; charset=utf-8")]))
          validated = svc & withValidateContentType "application/json"
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ show err

    it "rejects non-matching content type" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200
              [("Content-Type", "text/html")]))
          validated = svc & withValidateContentType "application/json"
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Left (CustomError _) -> pure ()
        other -> expectationFailure $ "Expected CustomError, got: " ++ show other

    it "rejects missing content type" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200 []))
          validated = svc & withValidateContentType "application/json"
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Left (CustomError msg) -> msg `shouldBe` "Missing Content-Type header"
        other -> expectationFailure $ "Expected CustomError, got: " ++ show other

  describe "withValidateHeader" $ do
    it "passes when header is present" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200
              [("X-Request-ID", "abc")]))
          validated = svc & withValidateHeader "X-Request-ID"
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ show err

    it "rejects when header is missing" $ do
      let svc = Service $ \_ -> pure (Right (fakeResponseWith HTTP.status200 []))
          validated = svc & withValidateHeader "X-Request-ID"
      req <- HTTP.parseRequest "http://example.com"
      result <- runService validated req
      case result of
        Left (CustomError _) -> pure ()
        other -> expectationFailure $ "Expected CustomError, got: " ++ show other

fakeResponseWith :: HTTP.Status -> [HTTP.Header] -> HttpResponse
fakeResponseWith status hdrs = HTTP.Response
  { HTTP.responseStatus = status
  , HTTP.responseVersion = HTTP.http11
  , HTTP.responseHeaders = hdrs
  , HTTP.responseBody = ""
  , HTTP.responseCookieJar = HTTP.createCookieJar []
  , HTTP.responseClose' = HTTP.ResponseClose (pure ())
  , HTTP.responseOriginalRequest = error "not used"
  , HTTP.responseEarlyHints = []
  }
