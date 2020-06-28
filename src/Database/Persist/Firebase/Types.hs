{-# LANGUAGE ExistentialQuantification #-}

module Database.Persist.Firebase.Types where

import qualified Data.Text as T
import Web.HttpApiData (ToHttpApiData)


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
              | Empty

data FbOrderBy = Child T.Text
               | Key
               | Val

data FbParam = forall q. ToHttpApiData q => FbParam q

data FbLimit = ToFirst Int | ToLast Int
