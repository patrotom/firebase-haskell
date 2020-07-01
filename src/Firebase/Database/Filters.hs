{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Filters
  ( filterParams
  , encodeQueryParam
  ) where

import Web.HttpApiData (ToHttpApiData, toQueryParam)
import Firebase.Database.Types
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Simple as S
import qualified Data.ByteString.Char8 as C8


filterParams :: Filter -> S.Query
filterParams EmptyFilter = []
filterParams Shallow = encodeQueryParam "shallow" True
filterParams (ComplexFilter ob sa ea et lm) =
  filterOrderBy ob ++
  filterStartAt sa ++
  filterEndAt ea   ++
  filterEqualTo et ++
  filterLimit lm

filterOrderBy :: Maybe OrderBy -> S.Query
filterOrderBy Nothing = []
filterOrderBy (Just ob) = encodeQueryParam "orderBy" t
  where t = case ob of
              Child x -> x
              Key     -> "$key"
              Val     -> "$value"

filterStartAt :: Maybe Param -> S.Query
filterStartAt Nothing = []
filterStartAt (Just (Param sa)) = encodeQueryParam "startAt" sa

filterEndAt :: Maybe Param -> S.Query
filterEndAt Nothing = []
filterEndAt (Just (Param ea)) = encodeQueryParam "endAt" ea

filterEqualTo :: Maybe Param -> S.Query
filterEqualTo Nothing = []
filterEqualTo (Just (Param et)) = encodeQueryParam "equalTo" et

filterLimit :: Maybe FbLimit -> S.Query
filterLimit Nothing = []
filterLimit (Just (ToFirst x)) = encodeQueryParam "limitToFirst" x
filterLimit (Just (ToLast x)) = encodeQueryParam "limitToLast" x

encodeQueryParam :: ToHttpApiData a => String -> a -> S.Query
encodeQueryParam k v = [(C8.pack k, Just $ E.encodeUtf8 . toQueryParam $ v)]
