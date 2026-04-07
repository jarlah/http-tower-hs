{-# LANGUAGE OverloadedStrings #-}

module Tower.Middleware.TransformSpec (spec) where

import Data.IORef
import Test.Hspec

import Tower.Service
import Tower.Error
import Tower.Error.Testing ()
import Data.Function ((&))
import Tower.Middleware.Transform

spec :: Spec
spec = describe "Transform middleware (generic)" $ do
  describe "withMapRequestPure" $ do
    it "transforms the request before passing to inner service" $ do
      let svc :: Service String String
          svc = Service $ \req -> pure (Right req)
          transformed = svc & withMapRequestPure (++ "!")
      result <- runService transformed "hello"
      result `shouldBe` Right "hello!"

    it "does not alter the response" $ do
      let svc :: Service String String
          svc = Service $ \_ -> pure (Right "response")
          transformed = svc & withMapRequestPure (++ "!")
      result <- runService transformed "request"
      result `shouldBe` Right "response"

    it "composes multiple transforms (right-to-left)" $ do
      let svc :: Service String String
          svc = Service $ \req -> pure (Right req)
          transformed = svc
                      & withMapRequestPure (++ "1")
                      & withMapRequestPure (++ "2")
                      & withMapRequestPure (++ "3")
      result <- runService transformed "base"
      -- Middleware applies left-to-right: 1 applied first, then 2, then 3
      result `shouldBe` Right "base321"

  describe "withMapRequest" $ do
    it "transforms the request with IO" $ do
      counter <- newIORef (0 :: Int)
      let svc :: Service String String
          svc = Service $ \req -> pure (Right req)
          transformed = svc & withMapRequest (\req -> do
            n <- atomicModifyIORef' counter (\c -> (c + 1, c))
            pure (req ++ "-" ++ show n))
      r1 <- runService transformed "req"
      r2 <- runService transformed "req"
      r1 `shouldBe` Right "req-0"
      r2 `shouldBe` Right "req-1"

    it "passes through errors from inner service" $ do
      let svc :: Service String String
          svc = Service $ \_ -> pure (Left (CustomError "fail"))
          transformed = svc & withMapRequest (\req -> pure (req ++ "!"))
      result <- runService transformed "hello"
      result `shouldBe` Left (CustomError "fail")
