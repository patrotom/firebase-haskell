{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.FiltersSpec (spec) where

import Test.Hspec
import Firebase.Database (complexFilter)
import Firebase.Database.Types
import Firebase.Database.Filters
import qualified Data.ByteString.Char8 as C8


spec :: Spec
spec = do
  describe "filterParams" $ do
    it "works with empty filter" $
      filterParams EmptyFilter `shouldBe` []
    it "works with shallow filter" $
      filterParams Shallow `shouldBe` [("shallow", Just "true")]
    it "works with complex filters" $ do
      filterParams complexFilter1 `shouldBe` [("orderBy", Just "height")]
      filterParams complexFilter2 `shouldBe` [("orderBy", Just "$key")]
      filterParams complexFilter3 `shouldBe` [("orderBy", Just "$value")]
      filterParams complexFilter4 `shouldBe` [("startAt", Just "5")]
      filterParams complexFilter5 `shouldBe` [("startAt", Just "true")]
      filterParams complexFilter6 `shouldBe` [("startAt", Just "\"a\"")]
      filterParams complexFilter7 `shouldBe` [("endAt", Just "42")]
      filterParams complexFilter8 `shouldBe` [("endAt", Just "false")]
      filterParams complexFilter9 `shouldBe` [("endAt", Just "\"bvd\"")]


complexFilter1 :: Filter
complexFilter1 = complexFilter { fOrderBy = Just (Child "height") }

complexFilter2 :: Filter
complexFilter2 = complexFilter { fOrderBy = Just Key }

complexFilter3 :: Filter
complexFilter3 = complexFilter { fOrderBy = Just Val }

complexFilter4 :: Filter
complexFilter4 = complexFilter { fStartAt = Just (Param (5 :: Int)) }

complexFilter5 :: Filter
complexFilter5 = complexFilter { fStartAt = Just (Param True) }

complexFilter6 :: Filter
complexFilter6 = complexFilter { fStartAt = Just (Param (show "a")) }

complexFilter7 :: Filter
complexFilter7 = complexFilter { fEndAt = Just (Param (42 :: Int)) }

complexFilter8 :: Filter
complexFilter8 = complexFilter { fEndAt = Just (Param False) }

complexFilter9 :: Filter
complexFilter9 = complexFilter { fEndAt = Just (Param (show "bvd")) }
