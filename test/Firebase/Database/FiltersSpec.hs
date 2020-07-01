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
      filterParams complexFilter10 `shouldBe` [("equalTo", Just "50.5")]
      filterParams complexFilter11 `shouldBe` [("equalTo", Just "true")]
      filterParams complexFilter12 `shouldBe` [("equalTo", Just "\"abc\"")]
      filterParams complexFilter13 `shouldBe` [("limitToFirst", Just "42")]
      filterParams complexFilter14 `shouldBe` [("limitToLast", Just "42")]
      filterParams complexFilter15 `shouldBe` [ ("orderBy", Just "height")
                                              , ("startAt", Just "5")
                                              , ("endAt", Just "42")
                                              , ("equalTo", Just "38.5")
                                              , ("limitToFirst", Just "100")
                                              ]
  describe "encodeQueryParam" $ do
    it "encodes string parameter" $
      encodeQueryParam "p1" (show "val") `shouldBe` [("p1", Just "\"val\"")]
    it "encodes int parameter" $
      encodeQueryParam "p2" (5 :: Int) `shouldBe` [("p2", Just "5")]
    it "encodes float parameter" $
      encodeQueryParam "p3" (2.5 :: Float) `shouldBe` [("p3", Just "2.5")]
    it "encodes bool parameter" $
      encodeQueryParam "p4" True `shouldBe` [("p4", Just "true")]


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

complexFilter10 :: Filter
complexFilter10 = complexFilter { fEqualTo = Just (Param (50.5 :: Float)) }

complexFilter11 :: Filter
complexFilter11 = complexFilter { fEqualTo = Just (Param True) }

complexFilter12 :: Filter
complexFilter12 = complexFilter { fEqualTo = Just (Param (show "abc")) }

complexFilter13 :: Filter
complexFilter13 = complexFilter { fLimit = Just (ToFirst 42) }

complexFilter14 :: Filter
complexFilter14 = complexFilter { fLimit = Just (ToLast 42) }

complexFilter15 :: Filter
complexFilter15 = complexFilter { fOrderBy = Just (Child "height")
                                , fStartAt = Just (Param (5 :: Int))
                                , fEndAt   = Just (Param (42 :: Int))
                                , fEqualTo = Just (Param (38.5 :: Float))
                                , fLimit   = Just (ToFirst 100)
                                }
