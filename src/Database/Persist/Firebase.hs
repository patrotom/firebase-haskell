{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Firebase where

import Network.HTTP.Req
import Control.Monad
import GHC.Generics
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, encode, Value)
import qualified Data.ByteString.Char8 as B
import Database.Persist.Firebase.Types
import qualified Database.Persist.Firebase.Utils as U
import qualified Data.Map as M
import qualified Data.Text as T


fbRead :: (MonadHttp m, FromJSON r) => Url s    ->
                                       Option s ->
                                       m (JsonResponse r)
fbRead url = req GET url NoReqBody jsonResponse

fbWrite :: (MonadHttp m, FromJSON r, ToJSON b) => Url s         ->
                                                  Option s      ->
                                                  ReqBodyJson b ->
                                                  m (JsonResponse r)
fbWrite url aPar body = req PUT url body jsonResponse aPar

-- push = undefined

-- update = undefined

-- delete = undefined

fbReq :: ToJSON b => FbRequest   ->
                     FbConfig    ->
                     FbLocation  ->
                     FbQuery     ->
                     b           ->
                     IO Value
fbReq req conf loc qr body = runReq defaultHttpConfig $ do
  r <- fbReqP req conf loc qr body
  return (responseBody r :: Value)

fbReqP :: (MonadHttp m, ToJSON b, FromJSON r) => FbRequest   ->
                                                 FbConfig    ->
                                                 FbLocation  ->
                                                 FbQuery     ->
                                                 b           ->
                                                 m (JsonResponse r)
fbReqP req conf loc qr body =
  case req of
    Read  -> fbRead url par
    Write -> fbWrite url aPar (ReqBodyJson body)
  where url  = U.fbUrl (projectId conf) loc
        par  = U.fbParams (authToken conf) qr
        aPar = U.authParam (authToken conf)

-- =============================================================================

-- cQuery = ComplexQuery { qOrderBy = Just Key, qStartAt = Just (FbParam ("\"a\"" :: String)), qEndAt = Nothing, qEqualTo = Nothing, qLimit = Nothing }

-- fbTest :: IO Value
-- fbTest = fbReq Read FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs" Shallow NoReqBody
