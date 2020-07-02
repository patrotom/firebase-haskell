{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Filters
Description : Generating filter query parameters.
Copyright   : (c) Tomas Patro, 2020
License     : MIT
Maintainer  : tomas.patro@gmail.com
Stability   : experimental
Portability : POSIX

Functionality for generating query parameters which are used as filters in
Firebase Database requests.
-}
module Firebase.Database.Filters where

import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Firebase.Database.Types
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Simple as S
import qualified Data.ByteString.Char8 as C8


{-|
  Convert 'Filter' to a 'Network.HTTP.Simple.Query' which is
  @[(@'Data.ByteString.ByteString'@, @'Maybe'@ @'Data.ByteString.ByteString'@)]@.

  For 'EmptyFilter' filter it returns @[]@. For 'Shallow' filter it returns
  @["shallow", @'Just'@ "true"]@. If you pass 'ComplexFilter', it returns
  concatenation of 'filterOrderBy', 'filterStartAt', 'filterEndAt',
  'filterEqualTo', and 'filterLimit'.
-}
filterParams :: Filter -> S.Query
filterParams EmptyFilter = []
filterParams Shallow = encodeQueryParam "shallow" True
filterParams (ComplexFilter ob sa ea et lm) =
  filterOrderBy ob ++
  filterStartAt sa ++
  filterEndAt ea   ++
  filterEqualTo et ++
  filterLimit lm


{-|
  Convert 'Maybe' 'OrderBy' to a 'Network.HTTP.Simple.Query'.
  
  If the first argument is 'Nothing', it returns in @[]@.
  
  If the first argument is 'Just' 'OrderBy', it returns
  'encodeQueryParam' @"orderBy" (@'show'@ x)@ for 'Child' @x@,
  'encodeQueryParam'@ "orderBy" (@'show' @"$key")@ for 'Key', and
  'encodeQueryParam'@ "orderBy" (@'show' @"$value")@ for 'Val'.
-}
filterOrderBy :: Maybe OrderBy -> S.Query
filterOrderBy Nothing = []
filterOrderBy (Just ob) = encodeQueryParam "orderBy" (show t)
  where t = case ob of
              Child x -> x
              Key     -> "$key"
              Val     -> "$value"

{-|
  Convert 'Maybe' 'Param' to a 'Network.HTTP.Simple.Query'.
  
  If the first argument is 'Nothing', it returns in @[]@.
  
  If the first argument is 'Just' 'Param' it returns
  'encodeQueryParam'@ "startAt" sa@.
-}
filterStartAt :: Maybe Param -> S.Query
filterStartAt Nothing = []
filterStartAt (Just (Param sa)) = encodeQueryParam "startAt" sa


{-|
  Convert 'Maybe' 'Param' to a 'Network.HTTP.Simple.Query'.
  
  If the first argument is 'Nothing', it returns in @[]@.
  
  If the first argument is 'Just' 'Param' it returns
  'encodeQueryParam'@ "endAt" ea@.
-}
filterEndAt :: Maybe Param -> S.Query
filterEndAt Nothing = []
filterEndAt (Just (Param ea)) = encodeQueryParam "endAt" ea

{-|
  Convert 'Maybe' 'Param' to a 'Network.HTTP.Simple.Query'.
  
  If the first argument is 'Nothing', it returns in @[]@.
  
  If the first argument is 'Just' 'Param' it returns
  'encodeQueryParam'@ "equalTo" et@.
-}
filterEqualTo :: Maybe Param -> S.Query
filterEqualTo Nothing = []
filterEqualTo (Just (Param et)) = encodeQueryParam "equalTo" et

{-|
  Convert 'Maybe' 'FbLimit' to a 'Network.HTTP.Simple.Query'.
  
  If the first argument is 'Nothing', it returns in @[]@.
  
  If the first argument is 'Just' 'FbLimit', it returns
  'encodeQueryParam'@ "limitToFirst" x@ for 'Just'@ (@'ToFirst'@ x)@,
  and 'encodeQueryParam'@ "limitToLast" x@ for 'Just'@ (@'ToLast'@ x)@.
-}
filterLimit :: Maybe FbLimit -> S.Query
filterLimit Nothing = []
filterLimit (Just (ToFirst x)) = encodeQueryParam "limitToFirst" x
filterLimit (Just (ToLast x)) = encodeQueryParam "limitToLast" x


{-|
  Take name and value of a query parameter and encode it to a
  'Network.HTTP.Simple.Query'. Value has to be an instance of
  'Web.HttpApiData.ToHttpApiData' type class.
-}
encodeQueryParam :: ToHttpApiData a =>
                    String     -- ^ Name of the query parameter
                    -> a       -- ^ Value of the query parameter
                    -> S.Query -- ^ Resulting encoded query parameter of the type @[(@'Data.ByteString.ByteString'@, @'Maybe'@ @'Data.ByteString.ByteString'@)]@
encodeQueryParam k v = [(C8.pack k, Just $ E.encodeUtf8 . toQueryParam $ v)]
