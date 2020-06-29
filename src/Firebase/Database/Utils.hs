{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Utils where

import Network.HTTP.Req
import Web.HttpApiData (ToHttpApiData)
import Firebase.Database.Types
import Firebase.Database.Filters (filterParams)
import qualified Data.Text as T


baseUrl :: T.Text
baseUrl = ".firebaseio.com"

dbUrl :: T.Text -> T.Text -> Url 'Https
dbUrl pId loc = https (pId `T.append` baseUrl) /: (loc `T.append` ".json")

dbParams :: (QueryParam p, Semigroup p, Monoid p) => Maybe FbAuthToken -> Filter -> p
dbParams tok qr = authParam tok <> filterParams qr

authParam :: (QueryParam p, Monoid p) => Maybe FbAuthToken -> p
authParam Nothing = mempty
authParam (Just tok) =
  case tok of
    OAuth2 t  -> "access_token" =: t
    IdToken t -> "auth" =: t
