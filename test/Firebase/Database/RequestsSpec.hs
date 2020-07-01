{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.RequestsSpec (spec) where

import Test.Hspec
import Firebase.Database.Types
import Firebase.Database.Requests
import Firebase.Database (complexFilter)
import Data.Aeson (ToJSON, encode, toJSON, (.=), object)
import qualified Firebase.Database.Utils as U
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Simple as S
import qualified Data.ByteString.Lazy as L


spec :: Spec
spec = do
  describe "commonRequest" $
    it "generates valid common request" $ do
      C.path commReq `shouldBe` snd dbUrl
      C.host commReq `shouldBe` fst dbUrl
      C.queryString commReq `shouldBe` "?access_token=123456&orderBy=%22height%22&startAt=5&endAt=42&equalTo=38.5&limitToFirst=100"
      C.port commReq `shouldBe` 443
      C.secure commReq `shouldBe` True
  describe "fbRead" $
    it "generates valid read GET request" $
      C.method readReq `shouldBe` "GET"
  describe "fbWrite" $
    it "generates valid write PUT request" $ do
      C.method writeReq1 `shouldBe` "PUT"
      extractBody (C.requestBody writeReq1) `shouldBe` encode person
      extractBody (C.requestBody writeReq2) `shouldBe` ""
  describe "fbPush" $
    it "generates valid push POST request" $ do
      C.method pushReq1 `shouldBe` "POST"
      extractBody (C.requestBody pushReq1) `shouldBe` encode person
      extractBody (C.requestBody pushReq2) `shouldBe` ""
  describe "fbUpdate" $
    it "generates valid update PATCH request" $ do
      C.method updateReq1 `shouldBe` "PATCH"
      extractBody (C.requestBody updateReq1) `shouldBe` encode person
      extractBody (C.requestBody updateReq2) `shouldBe` ""
  describe "fbDelete" $
    it "generates valid delete DELETE request" $
      C.method deleteReq `shouldBe` "DELETE"


data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

dbUrl :: DbURL
dbUrl = U.dbUrl "example-project" "random/path"

fbAuthToken :: FbAuthToken
fbAuthToken = OAuth2 "123456"

complexFilter1 :: Filter
complexFilter1 = complexFilter { fOrderBy = Just (Child "height")
                               , fStartAt = Just (Param (5 :: Int))
                               , fEndAt   = Just (Param (42 :: Int))
                               , fEqualTo = Just (Param (38.5 :: Float))
                               , fLimit   = Just (ToFirst 100)
                               }
dbParams :: S.Query
dbParams = U.dbParams (Just fbAuthToken) complexFilter1

person :: Person
person = Person "Thomas" 23

reqBody :: RequestBody
reqBody = Body person

commReq :: S.Request
commReq = commonRequest dbUrl dbParams

readReq :: S.Request
readReq = fbRead dbUrl dbParams

writeReq1 :: S.Request
writeReq1 = fbWrite dbUrl dbParams reqBody

writeReq2 :: S.Request
writeReq2 = fbWrite dbUrl dbParams EmptyBody

pushReq1 :: S.Request
pushReq1 = fbPush dbUrl dbParams reqBody

pushReq2 :: S.Request
pushReq2 = fbPush dbUrl dbParams EmptyBody

updateReq1 :: S.Request
updateReq1 = fbUpdate dbUrl dbParams reqBody

updateReq2 :: S.Request
updateReq2 = fbUpdate dbUrl dbParams EmptyBody

deleteReq :: S.Request
deleteReq = fbDelete dbUrl dbParams

extractBody :: C.RequestBody -> L.ByteString
extractBody (C.RequestBodyLBS b) = b
