{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Filters where

import Network.HTTP.Req ((=:), QueryParam)
import Firebase.Database.Types
import qualified Data.Text as T


filterParams :: (QueryParam p, Semigroup p, Monoid p) => Filter -> p
filterParams EmptyFilter = mempty
filterParams Shallow = "shallow" =: True
filterParams (ComplexFilter ob sa ea et lm) =
  filterOrderBy ob <>
  filterStartAt sa <>
  filterEndAt ea   <>
  filterEqualTo et <>
  filterLimit lm

filterOrderBy :: (QueryParam p, Monoid p) => Maybe OrderBy -> p
filterOrderBy Nothing = mempty
filterOrderBy (Just ob) = "orderBy" =: show t
  where t = case ob of
              Child x -> x
              Key     -> T.pack "$key"
              Val     -> T.pack "$value"

filterStartAt :: (QueryParam p, Monoid p) => Maybe Param -> p
filterStartAt Nothing = mempty
filterStartAt (Just (Param sa)) = "startAt" =: sa

filterEndAt :: (QueryParam p, Monoid p) => Maybe Param -> p
filterEndAt Nothing = mempty
filterEndAt (Just (Param ea)) = "endAt" =: ea

filterEqualTo :: (QueryParam p, Monoid p) => Maybe Param -> p
filterEqualTo Nothing = mempty
filterEqualTo (Just (Param et)) = "equalTo" =: et

filterLimit :: (QueryParam p, Monoid p) => Maybe FbLimit -> p
filterLimit Nothing = mempty
filterLimit (Just (ToFirst x)) = "limitToFirst" =: x
filterLimit (Just (ToLast x)) = "limitToLast" =: x
