{-# LANGUAGE ExistentialQuantification #-}

{-|
  Module      : Firebase.Database.Types
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

  @
&#x7b;-\# LANGUAGE OverloadedStrings \#-&#x7d;

rootPath = ""
nestedPath = "some\/random\/example\/path"
  @
-}
type DbLocation = T.Text

{-|
  Database URL which points to the concrete data. First argument is the host
  and second argument is the path. It is used to construct a request.

  ==== __Examples__

  @
&#x7b;-\# LANGUAGE OverloadedStrings \#-&#x7d;
  
dbUrl = ("example-project.firebaseio.com", "\/some\/random\/example\/path.json")
  @
-}
type DbURL = (C8.ByteString, C8.ByteString)


-- | Firebase Database method to be performed by the request.
data DbMethod
  -- | @GET@ request for reading data
  = Read
  -- | @PUT@ request for writing data
  | Write
  -- | @POST@ request for pushing data
  | Push
  -- | @PATCH@ request for updating data
  | Update
  -- | @DELETE@ request for deleteing data
  | Delete
  deriving (Eq)

{-|
  Authentication token which is used to authenticate requests.
  
  Using 'OAuth2' will add @"access_token=\<ACCESS_TOKEN\>"@ query parameter to the
  URL. Using 'IdToken' (Firebase ID token) will add @"auth=\<ID_TOKEN\>"@ query
  paramter to the URL.

  For more information see how to [Authenticate REST Requests](https://firebase.google.com/docs/database/rest/auth).
-}
data FbAuthToken = OAuth2 T.Text | IdToken T.Text

{-|
  Main Firebase Database configuration. 
-}
data DbConfig =
  DbConfig { -- | Your [Firebase Project ID](https://firebase.google.com/docs/projects/learn-more)
             projectId :: T.Text,
             -- | Optional field determing whether to use or not to use authentication and which type of authentication to use
             authToken :: Maybe FbAuthToken 
           }

{-|
  These filters are represented in a request as the query parameters. Remember
  that filtering is server-side. If you, for example, use 'fOrderBy' filter,
  it is highly probable that the response will not be ordered because of the
  nature of @JSON@.

  Also remember that in order to use 'ComplexFilter' you have to setup index
  rules in your Firebase Database Console.

  For more information see [Filtering Data](https://firebase.google.com/docs/database/rest/retrieve-data#section-rest-filtering)
  section of Firebase Database REST API Documentation.
-}
data Filter
  -- | @shallow@ filter will replace all values of the keys by @true@ - it is used
  -- to determine the structure of large data sets
  = Shallow
  -- | You can combine these complex filters in any reasonable way you want.
  -- 'OrderBy' is a mandatory complex filter that needs to be set to use other
  -- complex filters.
  | ComplexFilter { -- | @orderBy@ filter used for server-side ordering
                    fOrderBy :: OrderBy,
                    -- | @startAt@ filter determines where to start in data
                    fStartAt :: Maybe Param,
                    -- | @endtAt@ filter determines where to end in data
                    fEndAt :: Maybe Param,
                    -- | @equalTo@ filter matches data by the given 'Param'
                    fEqualTo :: Maybe Param,
                    -- | @limitToFirst@\/@limitToLast@ filter limits the number of fetched records 
                    fLimit :: Maybe FbLimit
                  }
  -- | Option to not to use any filter
  | EmptyFilter

-- | Request body for @PUT@, @POST@, and @PATCH@ requests.
data RequestBody
  -- | Body needs to be a data type which can be encoded as a JSON
  = forall b. ToJSON b => Body b
  -- | Option to not specify the body of a request
  | EmptyBody

-- | Options on how to server-side order data.
data OrderBy
  -- | Order by a specific string 
  = Child T.Text
  -- | Order by the keys
  | Key
  -- | Order by the values
  | Val

{-|
  Polymorphic query parameter value.

  Note that in order to pass string value as a query parameter value, you have
  to surround it with @""@. You can use 'show' function for this purpose.
-}
data Param = forall q. ToHttpApiData q => Param q

{-|
  Type of a complex limit filter.
-}
data FbLimit
  -- | Processed as @limitToFirst@ query parameter
  = ToFirst Int
  -- | Processed as @limitToLast@ query parameter
  | ToLast Int
