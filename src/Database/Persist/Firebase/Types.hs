module Database.Persist.Firebase.Types where

import qualified Data.Text as T


data FbRequest = Read | Write | Push | Update | Delete
               deriving (Eq)

data FbAuthToken = OAuth2 T.Text | IdToken T.Text

data FbConfig = FbConfig
              { projectId :: T.Text
              , authToken :: Maybe FbAuthToken 
              }

type Location = T.Text
