{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Firebase.Filters where

import qualified Data.Text as T
import Network.HTTP.Req (https, (=:), Url, QueryParam)
import Web.HttpApiData (ToHttpApiData)
import Database.Persist.Firebase.Types


filterParams :: (QueryParam p, Semigroup p, Monoid p) => FbQuery -> p
filterParams EmptyQuery = mempty
filterParams Shallow = "shallow" =: True
filterParams (ComplexQuery ob sa ea et lm) =
  filterOrderBy ob <>
  filterStartAt sa <>
  filterEndAt ea   <>
  filterEqualTo et <>
  filterLimit lm

filterOrderBy :: (QueryParam p, Monoid p) => Maybe FbOrderBy -> p
filterOrderBy Nothing = mempty
filterOrderBy (Just ob) = "orderBy" =: show t
  where t = case ob of
              Child x -> x
              Key     -> T.pack "$key"
              Val     -> T.pack "$value"

filterStartAt :: (QueryParam p, Monoid p) => Maybe FbParam -> p
filterStartAt Nothing = mempty
filterStartAt (Just (FbParam sa)) = "startAt" =: sa

filterEndAt :: (QueryParam p, Monoid p) => Maybe FbParam -> p
filterEndAt Nothing = mempty
filterEndAt (Just (FbParam ea)) = "endAt" =: ea

filterEqualTo :: (QueryParam p, Monoid p) => Maybe FbParam -> p
filterEqualTo Nothing = mempty
filterEqualTo (Just (FbParam et)) = "equalTo" =: et

filterLimit :: (QueryParam p, Monoid p) => Maybe FbLimit -> p
filterLimit Nothing = mempty
filterLimit (Just (ToFirst x)) = "limitToFirst" =: x
filterLimit (Just (ToLast x)) = "limitToLast" =: x
