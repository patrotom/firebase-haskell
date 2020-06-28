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


fbRead :: (MonadHttp m, FromJSON r) => Url s -> m (JsonResponse r)
fbRead url = req GET url NoReqBody jsonResponse mempty

-- write = undefined

-- push = undefined

-- update = undefined

-- delete = undefined

fbReq :: (MonadHttp m, HttpBody b, FromJSON r) => FbRequest   ->
                                                  FbConfig    ->
                                                  FbLocation  ->
                                                  b           ->
                                                  m (JsonResponse r)
fbReq req conf loc body =
  case req of
    Read -> fbRead url
  where url = https (projectId conf) /: loc

-- =============================================================================

newtype MyData = MyData { name :: String } deriving (Show, Generic)

fbTest :: (MonadHttp m, FromJSON r) => m (JsonResponse r)
fbTest = fbReq Read FbConfig { projectId = "persistent-firebase.firebaseio.com", authToken = Nothing } "tests.json" NoReqBody

instance ToJSON MyData
instance FromJSON MyData

fbRun :: IO ()
fbRun = runReq defaultHttpConfig $ do
  r <- fbTest
  liftIO $ print (responseBody r :: Value)
