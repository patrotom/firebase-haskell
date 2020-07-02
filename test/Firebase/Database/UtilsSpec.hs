{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.UtilsSpec (spec) where

import Test.Hspec
import Firebase.Database.Types
import Firebase.Database.Filters
import Firebase.Database.Utils
import Firebase.Database (complexFilter)


spec :: Spec
spec = do
  describe "dbUrl" $ do
    it "generates valid URL" $
      dbUrl "test-example" "some/random/location" `shouldBe`
            ("test-example.firebaseio.com", "/some/random/location.json")
    it "generates valid root URL" $
      dbUrl "test-example" "" `shouldBe`
            ("test-example.firebaseio.com", "/.json")
  describe "dbParams" $ do
    it "generates valid auth query param" $ do
      dbParams (Just oAuth2Tok) EmptyFilter `shouldBe` [("access_token", Just "123456")]
      dbParams (Just idTok) EmptyFilter `shouldBe` [("auth", Just "987654")]
    it "does not generate auth query param when token is not provided" $
      dbParams Nothing EmptyFilter `shouldBe` []
    it "generates filter query params along with auth token" $
      dbParams (Just oAuth2Tok) complexFilter1 `shouldBe` [ ("access_token", Just "123456")
                                                          , ("orderBy", Just "\"$key\"")
                                                          , ("startAt", Just "5")
                                                          ]


oAuth2Tok :: FbAuthToken
oAuth2Tok = OAuth2 "123456"

idTok :: FbAuthToken
idTok = IdToken "987654"

complexFilter1 :: Filter
complexFilter1 = complexFilter { fStartAt = Just (Param (5 :: Int)) }
