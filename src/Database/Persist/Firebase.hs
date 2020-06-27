{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Firebase where

import Network.HTTP.Req
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Char8 as B
import Database.Persist.Firebase.Types


fbRead :: (MonadHttp m, FromJSON r) => Url s -> m (JsonResponse r)
fbRead url = req GET url NoReqBody jsonResponse mempty

-- write = undefined

-- push = undefined

-- update = undefined

-- delete = undefined

fbReq :: (MonadHttp m, HttpBody b, FromJSON r) => FbRequest -> FbConfig -> Location -> b -> m (JsonResponse r)
fbReq req conf loc body =
  case req of
    Read -> fbRead url
  where url = https (projectId conf) /: loc

fbTest :: (MonadHttp m, FromJSON r) => m (JsonResponse r)
fbTest = fbReq Read FbConfig { projectId = "persistent-firebase.firebaseio.com", authToken = Nothing } "users" NoReqBody
