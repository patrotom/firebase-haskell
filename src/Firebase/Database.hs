module Firebase.Database where

import Network.HTTP.Req
import Data.Aeson (FromJSON, ToJSON, encode, Value)
import Firebase.Database.Types
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Firebase.Database.Utils as U
import qualified Firebase.Database.Requests as FR


fbQuery :: FbRequest   ->
           FbConfig    ->
           FbLocation  ->
           FbQuery     ->
           FbBody      ->
           IO Value
fbQuery req conf loc qr body = runReq defaultHttpConfig $ do
  r <- fbQueryP req conf loc qr body
  return (responseBody r :: Value)

fbQueryP :: (MonadHttp m, FromJSON r) => FbRequest   ->
                                         FbConfig    ->
                                         FbLocation  ->
                                         FbQuery     ->
                                         FbBody      ->
                                         m (JsonResponse r)
fbQueryP req conf loc qr body =
  case req of
    Read   -> FR.fbRead url par
    Write  -> FR.fbWrite url aPar body
    Push   -> FR.fbPush url aPar body
    Update -> FR.fbUpdate url aPar body
    Delete -> FR.fbDelete url
  where url  = U.fbUrl (projectId conf) loc
        par  = U.fbParams (authToken conf) qr
        aPar = U.authParam (authToken conf)
