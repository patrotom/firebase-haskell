module Database.Persist.Firebase.Requests where

import Network.HTTP.Req
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Firebase.Types


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
