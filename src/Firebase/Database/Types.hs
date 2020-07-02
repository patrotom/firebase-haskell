{-# LANGUAGE ExistentialQuantification #-}

{-|
  Module      : Types
  Description : Common Firebase Database library data types.
  Copyright   : (c) Tomas Patro, 2020
  License     : MIT
  Maintainer  : tomas.patro@gmail.com
  Stability   : experimental
  Portability : POSIX

  Types which are used in the Firebase Database library. They define things like
  database location, configuration, request methods, and many more. 
-}
module Firebase.Database.Types where

import Web.HttpApiData (ToHttpApiData)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8


{-|
  Database location (path) of the concrete data. The path has to start without
  a @\/@ character at the beginning. Root path is defined as an empty string @""@.
  Do not add @".json"@ extension at the end of the path, since it is inserted
  automatically for you when a request is constructed.

  /Note:/ This is a path without an actual URL host. The final Firebase URL is
  represented by 'DbURL'.

  ==== __Examples__
  
  * __Root path__ - @""@
  * __Example path__ - @"some\/random\/example\/path"@
-}
type DbLocation = T.Text

type DbURL = (C8.ByteString, C8.ByteString)

data DbMethod = Read | Write | Push | Update | Delete
               deriving (Eq)

data FbAuthToken = OAuth2 T.Text | IdToken T.Text

data DbConfig = DbConfig
              { projectId :: T.Text
              , authToken :: Maybe FbAuthToken 
              }

data Filter = Shallow
            | ComplexFilter { fOrderBy :: Maybe OrderBy
                            , fStartAt :: Maybe Param
                            , fEndAt :: Maybe Param
                            , fEqualTo :: Maybe Param
                            , fLimit :: Maybe FbLimit
                            }
            | EmptyFilter

data RequestBody = forall b. ToJSON b => Body b
                 | EmptyBody

data OrderBy = Child T.Text
             | Key
             | Val

data Param = forall q. ToHttpApiData q => Param q

data FbLimit = ToFirst Int | ToLast Int
