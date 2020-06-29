{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Utils where

import qualified Data.Text as T
import Network.HTTP.Req
import Web.HttpApiData (ToHttpApiData)
import Firebase.Database.Types
import Firebase.Database.Filters (filterParams)


baseUrl :: T.Text
baseUrl = ".firebaseio.com"

fbUrl :: T.Text -> T.Text -> Url 'Https
fbUrl pId loc = https (pId `T.append` baseUrl) /: (loc `T.append` ".json")

fbParams :: (QueryParam p, Semigroup p, Monoid p) => Maybe FbAuthToken -> FbQuery -> p
fbParams tok qr = authParam tok <> filterParams qr

authParam :: (QueryParam p, Monoid p) => Maybe FbAuthToken -> p
authParam Nothing = mempty
authParam (Just tok) =
  case tok of
    OAuth2 t  -> "access_token" =: t
    IdToken t -> "auth" =: t
