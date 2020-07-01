{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.Requests where

import Firebase.Database.Types
import qualified Network.HTTP.Simple as S


fbRead :: DbURL -> S.Query -> S.Request
fbRead url qry = S.setRequestMethod "GET"
               $ commonRequest url qry

fbWrite :: DbURL -> S.Query -> RequestBody -> S.Request
fbWrite url qry EmptyBody = S.setRequestMethod "PUT"
                          $ commonRequest url qry
fbWrite url qry (Body b)  = S.setRequestMethod "PUT"
                          $ S.setRequestBodyJSON b
                          $ commonRequest url qry

fbPush :: DbURL -> S.Query -> RequestBody -> S.Request
fbPush url qry EmptyBody = S.setRequestMethod "POST"
                         $ commonRequest url qry
fbPush url qry (Body b)  = S.setRequestMethod "POST"
                         $ S.setRequestBodyJSON b
                         $ commonRequest url qry

fbUpdate :: DbURL -> S.Query -> RequestBody -> S.Request
fbUpdate url qry EmptyBody = S.setRequestMethod "PATCH"
                           $ commonRequest url qry
fbUpdate url qry (Body b)  = S.setRequestMethod "PATCH"
                           $ S.setRequestBodyJSON b
                           $ commonRequest url qry

fbDelete :: DbURL -> S.Request
fbDelete url = S.setRequestMethod "DELETE"
             $ commonRequest url []

commonRequest :: DbURL -> S.Query -> S.Request
commonRequest url qry = S.setRequestPath (snd url)
                      $ S.setRequestHost (fst url)
                      $ S.setRequestQueryString qry
                      $ S.setRequestPort 443
                      $ S.setRequestSecure True S.defaultRequest
