{-# LANGUAGE OverloadedStrings #-}

{-|
  Module      : Firebase.Database.Requests
  Description : Firebase Database REST requests.
  Copyright   : (c) Tomas Patro, 2020
  License     : MIT
  Maintainer  : tomas.patro@gmail.com
  Stability   : experimental
  Portability : POSIX

  You can use these functions to generate a particular request to Firebase Database
  or to generate a common request which contains the common attributes of rest of
  the requests.

  For more information see [Firebase Database REST API Documentation](https://firebase.google.com/docs/database/rest/start).
-}
module Firebase.Database.Requests where

import Firebase.Database.Types
import qualified Network.HTTP.Simple as S


-- | Generate Firebase Database @GET@ request for reading data.
fbRead :: DbURL -> S.Query -> S.Request
fbRead url qry = S.setRequestMethod "GET"
               $ commonRequest url qry

-- | Generate Firebase Database @PUT@ request for writing data.
fbWrite :: DbURL -> S.Query -> RequestBody -> S.Request
fbWrite url qry EmptyBody = S.setRequestMethod "PUT"
                          $ commonRequest url qry
fbWrite url qry (Body b)  = S.setRequestMethod "PUT"
                          $ S.setRequestBodyJSON b
                          $ commonRequest url qry

-- | Generate Firebase Database @POST@ request for pushing data.
fbPush :: DbURL -> S.Query -> RequestBody -> S.Request
fbPush url qry EmptyBody = S.setRequestMethod "POST"
                         $ commonRequest url qry
fbPush url qry (Body b)  = S.setRequestMethod "POST"
                         $ S.setRequestBodyJSON b
                         $ commonRequest url qry

-- | Generate Firebase Database @PATCH@ request for updating data.
fbUpdate :: DbURL -> S.Query -> RequestBody -> S.Request
fbUpdate url qry EmptyBody = S.setRequestMethod "PATCH"
                           $ commonRequest url qry
fbUpdate url qry (Body b)  = S.setRequestMethod "PATCH"
                           $ S.setRequestBodyJSON b
                           $ commonRequest url qry

-- | Generate Firebase Database @DELETE@ request for deleting data.
fbDelete :: DbURL -> S.Query -> S.Request
fbDelete url qry = S.setRequestMethod "DELETE"
                 $ commonRequest url qry

{-|
  Generate a common request with the common attributes. It properly sets request
  path, request host, query string, port, and sets secure communication attribute.
  This function is intended to be used by the other request functions, but you
  can take advantage of it and use it in a custom way. 
-}
commonRequest :: DbURL -> S.Query -> S.Request
commonRequest url qry = S.setRequestPath (snd url)
                      $ S.setRequestHost (fst url)
                      $ S.setRequestQueryString qry
                      $ S.setRequestPort 443
                      $ S.setRequestSecure True S.defaultRequest
