{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database where

import Data.Aeson (Value)
import Firebase.Database.Types
import qualified Data.Text as T
import qualified Network.HTTP.Simple as S
import qualified Firebase.Database.Utils as U
import qualified Firebase.Database.Requests as FR


dbQuery :: DbMethod    ->
           DbConfig    ->
           DbLocation  ->
           Filter      ->
           RequestBody ->
           IO Value
dbQuery req conf loc qr body = do
  let request = dbQueryP req conf loc qr body
  response <- S.httpJSON request
  return (S.getResponseBody response :: Value)

dbQueryP :: DbMethod    ->
            DbConfig    ->
            DbLocation  ->
            Filter      ->
            RequestBody ->
            S.Request
dbQueryP req conf loc qr body =
  case req of
    Read   -> FR.fbRead url par
    Write  -> FR.fbWrite url par body
    Push   -> FR.fbPush url par body
    Update -> FR.fbUpdate url par body
    Delete -> FR.fbDelete url
  where url  = U.dbUrl (projectId conf) loc
        par  = U.dbParams (authToken conf) qr

complexFilter :: Filter
complexFilter = ComplexFilter { fOrderBy = Nothing
                              , fStartAt = Nothing
                              , fEndAt   = Nothing
                              , fEqualTo = Nothing
                              , fLimit   = Nothing
                              }
