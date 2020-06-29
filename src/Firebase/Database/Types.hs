{-# LANGUAGE ExistentialQuantification #-}

module Firebase.Database.Types where

import Web.HttpApiData (ToHttpApiData)
import Data.Aeson (ToJSON)
import qualified Data.Text as T


type DbLocation = T.Text

data DbMethod = Read | Write | Push | Update | Delete
               deriving (Eq)

data FbAuthToken = OAuth2 T.Text | IdToken T.Text

data DbConfig = DbConfig
              { projectId :: T.Text
              , authToken :: Maybe FbAuthToken 
              }

data Filter = Shallow
            | ComplexFilter { fOrderby :: Maybe OrderBy
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
