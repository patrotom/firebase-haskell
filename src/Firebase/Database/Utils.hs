{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Utils where

import Web.HttpApiData (ToHttpApiData)
import Firebase.Database.Types
import Firebase.Database.Filters (filterParams, encodeQueryParam)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Network.HTTP.Simple as S


baseUrl :: T.Text
baseUrl = ".firebaseio.com"

dbUrl :: T.Text -> DbLocation -> DbURL
dbUrl pId loc = (E.encodeUtf8 (pId `T.append` baseUrl),
                 E.encodeUtf8 (loc `T.append` ".json"))

dbParams :: Maybe FbAuthToken -> Filter -> S.Query
dbParams tok qr = authParam tok ++ filterParams qr

authParam :: Maybe FbAuthToken -> S.Query
authParam Nothing = []
authParam (Just tok) =
  case tok of
    OAuth2 t  -> encodeQueryParam "access_token" t
    IdToken t -> encodeQueryParam "auth" t
