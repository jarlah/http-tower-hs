{-# LANGUAGE OverloadedStrings #-}

module Tower.ServiceSpec (spec) where

import Test.Hspec

import Control.Arrow (Arrow(..), ArrowChoice(..))
import qualified Control.Category
import Control.Category ((>>>))
import Tower.Service
import Tower.Error
import Data.Function ((&))
import Tower.Error.Testing ()

spec :: Spec
spec = describe "Core" $ do
  describe "Service" $ do
    it "runs a simple service" $ do
      let svc = Service $ \n -> pure (Right (n * 2 :: Int))
      result <- runService svc 21
      result `shouldBe` Right 42

    it "returns errors in Left" $ do
      let svc :: Service () String
          svc = Service $ \_ -> pure (Left (CustomError "boom"))
      result <- runService svc ()
      result `shouldBe` Left (CustomError "boom")

  describe "mapService" $ do
    it "transforms successful responses" $ do
      let svc :: Service () Int
          svc = Service $ \_ -> pure (Right 10)
          mapped = svc & mapService (* 3)
      result <- runService mapped ()
      result `shouldBe` Right 30

    it "passes through errors unchanged" $ do
      let svc :: Service () Int
          svc = Service $ \_ -> pure (Left TimeoutError)
          mapped = svc & mapService (* 3)
      result <- runService mapped ()
      result `shouldBe` Left TimeoutError

  describe "Category" $ do
    it "id passes through unchanged" $ do
      let svc = Control.Category.id :: Service Int Int
      result <- runService svc 42
      result `shouldBe` Right 42

    it "composes services with >>>" $ do
      let double = Service $ \n -> pure (Right (n * 2 :: Int))
          addOne = Service $ \n -> pure (Right (n + 1 :: Int))
          composed = double >>> addOne
      result <- runService composed 10
      result `shouldBe` Right 21

    it "short-circuits on error in first service" $ do
      let failing :: Service Int Int
          failing = Service $ \_ -> pure (Left (CustomError "fail"))
          addOne = Service $ \n -> pure (Right (n + 1 :: Int))
          composed = failing >>> addOne
      result <- runService composed 10
      result `shouldBe` Left (CustomError "fail")

    it "short-circuits on error in second service" $ do
      let double = Service $ \n -> pure (Right (n * 2 :: Int))
          failing :: Service Int Int
          failing = Service $ \_ -> pure (Left (CustomError "fail"))
          composed = double >>> failing
      result <- runService composed 10
      result `shouldBe` Left (CustomError "fail")

  describe "Arrow" $ do
    it "arr lifts a pure function" $ do
      let svc = arr (* 3) :: Service Int Int
      result <- runService svc 7
      result `shouldBe` Right 21

    it "first applies to the first element of a pair" $ do
      let double = Service $ \n -> pure (Right (n * 2 :: Int))
          paired = first double
      result <- runService paired (10, "hello")
      result `shouldBe` Right (20, "hello")

    it "first propagates errors" $ do
      let failing :: Service Int Int
          failing = Service $ \_ -> pure (Left TimeoutError)
          paired = first failing
      result <- runService paired (10, "hello")
      result `shouldBe` Left TimeoutError

  describe "ArrowChoice" $ do
    it "left applies to Left values" $ do
      let double = Service $ \n -> pure (Right (n * 2 :: Int))
          choice = left double :: Service (Either Int String) (Either Int String)
      result <- runService choice (Left 5)
      result `shouldBe` Right (Left 10)

    it "left passes through Right values" $ do
      let double = Service $ \n -> pure (Right (n * 2 :: Int))
          choice = left double :: Service (Either Int String) (Either Int String)
      result <- runService choice (Right "hello")
      result `shouldBe` Right (Right "hello")

    it "left propagates errors" $ do
      let failing :: Service Int Int
          failing = Service $ \_ -> pure (Left (CustomError "boom"))
          choice = left failing :: Service (Either Int String) (Either Int String)
      result <- runService choice (Left 5)
      result `shouldBe` Left (CustomError "boom")

  describe "composeMiddleware" $ do
    it "applies outer then inner" $ do
      let addTag tag (Service run) = Service $ \req ->
            run (req ++ tag)
          mw1 = addTag "[1]"
          mw2 = addTag "[2]"
          composed = composeMiddleware mw1 mw2
          svc = Service $ \req -> pure (Right req)
      result <- runService (composed svc) "start"
      result `shouldBe` Right "start[1][2]"
