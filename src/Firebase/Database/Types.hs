{-# LANGUAGE ExistentialQuantification #-}

module Firebase.Database.Types where

import Web.HttpApiData (ToHttpApiData)
import Data.Aeson (ToJSON)
import qualified Data.Text as T


type FbLocation = T.Text

data FbRequest = Read | Write | Push | Update | Delete
               deriving (Eq)

data FbAuthToken = OAuth2 T.Text | IdToken T.Text

data FbConfig = FbConfig
              { projectId :: T.Text
              , authToken :: Maybe FbAuthToken 
              }

data FbQuery = Shallow
             | ComplexQuery { qOrderBy :: Maybe FbOrderBy
                            , qStartAt :: Maybe FbParam
                            , qEndAt :: Maybe FbParam
                            , qEqualTo :: Maybe FbParam
                            , qLimit :: Maybe FbLimit
                            }
              | EmptyQuery

data FbBody = forall b. ToJSON b => Body b
            | EmptyBody

data FbOrderBy = Child T.Text
               | Key
               | Val

data FbParam = forall q. ToHttpApiData q => FbParam q

data FbLimit = ToFirst Int | ToLast Int
