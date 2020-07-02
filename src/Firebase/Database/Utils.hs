{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Firebase.Database.Utils
  Description : Utility functions.
  Copyright   : (c) Tomas Patro, 2020
  License     : MIT
  Maintainer  : tomas.patro@gmail.com
  Stability   : experimental
  Portability : POSIX

  Helper utility functions used by the other modules. 
-}
module Firebase.Database.Utils
  ( dbUrl
  , dbParams
  ) where

import Web.HttpApiData (ToHttpApiData)
import Firebase.Database.Types
import Firebase.Database.Filters (filterParams, encodeQueryParam)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Network.HTTP.Simple as S


-- | Generate database URL from Project ID and data location.
dbUrl :: T.Text -- ^ Firebase Project ID
      -> DbLocation -- ^ Path to data in database
      -> DbURL -- ^ Final database URL
dbUrl pId loc = (E.encodeUtf8 (pId `T.append` baseUrl),
                 E.encodeUtf8 ("/" `T.append` loc `T.append` ".json"))

-- | Generate query parameters from authentication token and filters.
dbParams :: Maybe FbAuthToken -> Filter -> S.Query
dbParams tok qr = authParam tok ++ filterParams qr

baseUrl :: T.Text
baseUrl = ".firebaseio.com"

authParam :: Maybe FbAuthToken -> S.Query
authParam Nothing = []
authParam (Just tok) =
  case tok of
    OAuth2 t  -> encodeQueryParam "access_token" t
    IdToken t -> encodeQueryParam "auth" t
