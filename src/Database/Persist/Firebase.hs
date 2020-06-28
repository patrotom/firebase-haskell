{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

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

fbWrite :: (MonadHttp m, FromJSON r) => Url s         ->
                                        Option s      ->
                                        FbBody        ->
                                        m (JsonResponse r)
fbWrite url aPar EmptyBody = req PUT url NoReqBody jsonResponse aPar
fbWrite url aPar (Body b)  = req PUT url (ReqBodyJson b) jsonResponse aPar

fbPush :: (MonadHttp m, FromJSON r) => Url s         ->
                                       Option s      ->
                                       FbBody        ->
                                       m (JsonResponse r)
fbPush url aPar EmptyBody = req POST url NoReqBody jsonResponse aPar
fbPush url aPar (Body b)  = req POST url (ReqBodyJson b) jsonResponse aPar

fbUpdate :: (MonadHttp m, FromJSON r) => Url s         ->
                                         Option s      ->
                                         FbBody        ->
                                         m (JsonResponse r)
fbUpdate url aPar EmptyBody = req PATCH url NoReqBody jsonResponse aPar
fbUpdate url aPar (Body b)  = req PATCH url (ReqBodyJson b) jsonResponse aPar

fbDelete :: (MonadHttp m, FromJSON r) => Url s -> m (JsonResponse r)
fbDelete url = req DELETE url NoReqBody jsonResponse mempty

fbReq :: FbRequest   ->
         FbConfig    ->
         FbLocation  ->
         FbQuery     ->
         FbBody      ->
         IO Value
fbReq req conf loc qr body = runReq defaultHttpConfig $ do
  r <- fbReqP req conf loc qr body
  return (responseBody r :: Value)

fbReqP :: (MonadHttp m, FromJSON r) => FbRequest   ->
                                       FbConfig    ->
                                       FbLocation  ->
                                       FbQuery     ->
                                       FbBody      ->
                                       m (JsonResponse r)
fbReqP req conf loc qr body =
  case req of
    Read   -> fbRead url par
    Write  -> fbWrite url aPar body
    Push   -> fbPush url aPar body
    Update -> fbUpdate url aPar body
    Delete -> fbDelete url
  where url  = U.fbUrl (projectId conf) loc
        par  = U.fbParams (authToken conf) qr
        aPar = U.authParam (authToken conf)

-- =============================================================================

cQuery = ComplexQuery { qOrderBy = Just Key, qStartAt = Just (FbParam ("\"a\"" :: String)), qEndAt = Nothing, qEqualTo = Nothing, qLimit = Nothing }

fbTest :: IO Value
fbTest = fbReq Read FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs" Shallow EmptyBody

data DinoInfo = DinoInfo { height :: Float, lengt :: Float, weight :: Float } deriving (Show, Generic)
newtype Dino = Dino { trex :: DinoInfo } deriving (Show, Generic)

instance ToJSON DinoInfo
instance FromJSON DinoInfo

instance ToJSON Dino
instance FromJSON Dino

myDino = Dino { trex = DinoInfo { height = 5, lengt = 6, weight = 100.5 } }

fbTest1 :: IO Value
fbTest1 = fbReq Write FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs" EmptyQuery (Body myDino)

fbTest2 :: IO Value
fbTest2 = fbReq Push FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs" EmptyQuery (Body myDino)

fbTest3 :: IO Value
fbTest3 = fbReq Update FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs/-MAvn3oHg6NTIwaB6R1c" EmptyQuery (Body DinoInfo { height = 10, lengt = 6, weight = 100.5 })

fbTest4 :: IO Value
fbTest4 = fbReq Delete FbConfig { projectId = "persistent-firebase", authToken = Nothing } "dinosaurs/-MAvn3oHg6NTIwaB6R1c/weight" EmptyQuery EmptyBody
