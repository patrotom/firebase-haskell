{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Firebase.Utils where

import qualified Data.Text as T
import Network.HTTP.Req (https, (=:), Url, QueryParam)
import Web.HttpApiData (ToHttpApiData)
import Database.Persist.Firebase.Types
import Database.Persist.Firebase.Filters (filterParams)


baseUrl :: T.Text
baseUrl = "firebaseio.com"

fbParams :: (QueryParam p, Semigroup p, Monoid p) => Maybe FbAuthToken -> FbQuery -> p
fbParams tok qr = authParam tok <> filterParams qr

authParam :: (QueryParam p, Monoid p) => Maybe FbAuthToken -> p
authParam Nothing = mempty
authParam (Just tok) =
  case tok of
    OAuth2 t  -> "access_token" =: t
    IdToken t -> "auth" =: t
