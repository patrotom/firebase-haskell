{-# LANGUAGE OverloadedStrings #-}

module Firebase.DatabaseSpec (spec) where

import Test.Hspec
import Data.Aeson (ToJSON, encode, toJSON, (.=), object)
import Firebase.Database
import qualified Network.HTTP.Simple as S
import qualified Network.HTTP.Client as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as E


spec :: Spec
spec =
  describe "dbQueryP" $ do
    it "constructs valid read GET request" $ do
      C.path readQuery `shouldBe` dbLocation'
      C.host readQuery `shouldBe` dbUrl
      C.queryString readQuery `shouldBe` "?access_token=123456789&shallow=true"
      C.port readQuery `shouldBe` 443
      C.secure readQuery `shouldBe` True
      C.method readQuery `shouldBe` "GET"
      extractBody (C.requestBody readQuery) `shouldBe` ""
    it "constructs valid write PUT request" $ do
      C.path writeQuery `shouldBe` dbLocation'
      C.host writeQuery `shouldBe` dbUrl
      C.queryString writeQuery `shouldBe` "?access_token=123456789"
      C.port writeQuery `shouldBe` 443
      C.secure writeQuery `shouldBe` True
      C.method writeQuery `shouldBe` "PUT"
      extractBody (C.requestBody writeQuery) `shouldBe` ""
    it "constructs valid push POST request" $ do
      C.path pushQuery `shouldBe` dbLocation'
      C.host pushQuery `shouldBe` dbUrl
      C.queryString pushQuery `shouldBe` "?access_token=123456789"
      C.port pushQuery `shouldBe` 443
      C.secure pushQuery `shouldBe` True
      C.method pushQuery `shouldBe` "POST"
      extractBody (C.requestBody pushQuery) `shouldBe` ""
    it "constructs valid update PATCH request" $ do
      C.path updateQuery `shouldBe` dbLocation'
      C.host updateQuery `shouldBe` dbUrl
      C.queryString updateQuery `shouldBe` "?access_token=123456789"
      C.port updateQuery `shouldBe` 443
      C.secure updateQuery `shouldBe` True
      C.method updateQuery `shouldBe` "PATCH"
      extractBody (C.requestBody updateQuery) `shouldBe` ""
    it "constructs valid delete DELETE request" $ do
      C.path deleteQuery `shouldBe` dbLocation'
      C.host deleteQuery `shouldBe` dbUrl
      C.queryString deleteQuery `shouldBe` "?access_token=123456789"
      C.port deleteQuery `shouldBe` 443
      C.secure deleteQuery `shouldBe` True
      C.method deleteQuery `shouldBe` "DELETE"
      extractBody (C.requestBody deleteQuery) `shouldBe` ""


dbConfig :: DbConfig
dbConfig = DbConfig { projectId = "example-project"
                    , authToken = Just (OAuth2 "123456789")
                    }

dbUrl :: C8.ByteString
dbUrl = E.encodeUtf8 (projectId dbConfig) `C8.append` ".firebaseio.com"

dbLocation :: DbLocation
dbLocation = "some/location"

dbLocation' :: C8.ByteString
dbLocation' = "/" `C8.append` E.encodeUtf8 dbLocation `C8.append` ".json"

qFilter :: Filter
qFilter = Shallow

readQuery :: S.Request
readQuery = dbQueryP Read dbConfig dbLocation qFilter EmptyBody

writeQuery :: S.Request
writeQuery = dbQueryP Write dbConfig dbLocation EmptyFilter EmptyBody

pushQuery :: S.Request
pushQuery = dbQueryP Push dbConfig dbLocation EmptyFilter EmptyBody

updateQuery :: S.Request
updateQuery = dbQueryP Update dbConfig dbLocation EmptyFilter EmptyBody

deleteQuery :: S.Request
deleteQuery = dbQueryP Delete dbConfig dbLocation EmptyFilter EmptyBody

extractBody :: C.RequestBody -> L.ByteString
extractBody (C.RequestBodyLBS b) = b
