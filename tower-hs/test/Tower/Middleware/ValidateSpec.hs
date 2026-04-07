{-# LANGUAGE OverloadedStrings #-}

module Tower.Middleware.ValidateSpec (spec) where

import Test.Hspec

import Tower.Service
import Tower.Error
import Tower.Error.Testing ()
import Data.Function ((&))
import Tower.Middleware.Validate

spec :: Spec
spec = describe "Validate middleware (generic)" $ do
  it "passes responses that return Nothing" $ do
    let svc :: Service () String
        svc = Service $ \_ -> pure (Right "ok")
        validated = svc & withValidate (const Nothing)
    result <- runService validated ()
    result `shouldBe` Right "ok"

  it "rejects responses that return Just" $ do
    let svc :: Service () String
        svc = Service $ \_ -> pure (Right "bad")
        validated = svc & withValidate (\_ -> Just "validation failed")
    result <- runService validated ()
    result `shouldBe` Left (CustomError "validation failed")

  it "passes through errors from inner service" $ do
    let svc :: Service () String
        svc = Service $ \_ -> pure (Left TimeoutError)
        validated = svc & withValidate (const (Just "should not trigger"))
    result <- runService validated ()
    result `shouldBe` Left TimeoutError

  it "can inspect the response value" $ do
    let svc :: Service () Int
        svc = Service $ \_ -> pure (Right 42)
        validated = svc & withValidate (\n -> if n > 100 then Just "too big" else Nothing)
    result <- runService validated ()
    result `shouldBe` Right 42

  it "rejects based on response value" $ do
    let svc :: Service () Int
        svc = Service $ \_ -> pure (Right 200)
        validated = svc & withValidate (\n -> if n > 100 then Just "too big" else Nothing)
    result <- runService validated ()
    result `shouldBe` Left (CustomError "too big")
