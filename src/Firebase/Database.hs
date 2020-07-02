{-|
  Module      : Firebase.Database
  Description : Generate and run Firebase Database operations.
  Copyright   : (c) Tomas Patro, 2020
  License     : MIT
  Maintainer  : tomas.patro@gmail.com
  Stability   : experimental
  Portability : POSIX

  A simple Haskell wrapper library for the [Firebase Database REST API](https://firebase.google.com/docs/reference/rest/database).

  This library currently supports basic @GET@, @PUT@, @POST@, @PATCH@, and
  @DELETE@ operations. See the example usage below to start using the library.

  = Example Usage

  First, this is a piece of boilerplate that should be in place before you try the examples:

  @
&#x7b;-\# LANGUAGE OverloadedStrings \#-&#x7d;

module Main (main) where

import Firebase.Database
import Data.Aeson


config = DbConfig { projectId = "your-project-id", authToken = Nothing }
  @

  Make a simple read @GET@ request to fetch data from the root location of your
  project:


  > main :: IO ()
  > main = do
  >   let method = Read
  >       filter = EmptyFilter
  >       requestBody = EmptyBody
  >       location = ""
  >   response <- dbQuery method config location filter requestBody
  >   print response

  Let's add some filters and nested location to this request. We take advantage
  of the prepared 'complexFilter':

  > main :: IO ()
  > main = do
  >   let method = Read
  >       filter = complexFilter { fOrderBy = Key
  >                              , fStartAt = Just $ Param (show "h")
  >                              }
  >       requestBody = EmptyBody
  >       location = "languages/functional"
  >   response <- dbQuery method config location filter requestBody
  >   print response

  You can also make @PUT@ ('Write'), @POST@ ('Push'), and @PATCH@ ('Update')
  requests with a defined request body:

  > data Person = Person String Int
  > instance ToJSON Person where
  >     toJSON (Person name age) = object
  >         [ "name" .= name
  >         , "age"  .= age
  >         ]
  > 
  > main :: IO ()
  > main = do
  >   let method = Push
  >       filter = EmptyFilter
  >       requestBody = Body (Person "Thomas" 23)
  >       location = "people"
  >   response <- dbQuery method config location filter requestBody
  >   print response

  You simply define the path and use @DELETE@ ('Delete') request to delete data
  at the location (path):

  > main :: IO ()
  > main = do
  >   let method = Delete
  >       filter = EmptyFilter
  >       requestBody = EmptyBody
  >       location = "dinosaurs/stegosaurus"
  >   response <- dbQuery method config location filter requestBody
  >   print response
-}

module Firebase.Database
  ( module Firebase.Database.Types
  , dbQuery
  , dbQueryP
  , complexFilter
  ) where

import Data.Aeson (Value)
import Firebase.Database.Types
import qualified Data.Text as T
import qualified Network.HTTP.Simple as S
import qualified Firebase.Database.Utils as U
import qualified Firebase.Database.Requests as FR


{-|
  Constructs a particular Firebase Database request based on the input attributes.

  It returns 'IO' action which contains 'Value' that can be further processed
  using 'Aeson' library.

  See the __Example Usage__ above to understand how this function works.
-}
dbQuery :: DbMethod -- ^ Type of Firebase Database operation
        -> DbConfig -- ^ Firebase Database Configuration
        -> DbLocation -- ^ Location (path) of the data 
        -> Filter -- ^ Filter used for server-side filtering of the data
        -> RequestBody -- ^ Request body data to be send 
        -> IO Value -- ^ IO action containing the response
dbQuery req conf loc fil body = do
  let request = dbQueryP req conf loc fil body
  response <- S.httpJSON request
  return (S.getResponseBody response :: Value)

{-|
  Similar to 'dbQuery' but it does not run the database request.

  It returns prepared 'S.Request' which can be consumed and run later.
-}
dbQueryP :: DbMethod
         -> DbConfig
         -> DbLocation
         -> Filter
         -> RequestBody
         -> S.Request
dbQueryP req conf loc fil body =
  case req of
    Read   -> FR.fbRead url par
    Write  -> FR.fbWrite url par body
    Push   -> FR.fbPush url par body
    Update -> FR.fbUpdate url par body
    Delete -> FR.fbDelete url par
  where url  = U.dbUrl (projectId conf) loc
        par  = U.dbParams (authToken conf) fil

{-|
  Prepared 'ComplexFilter' to ease the writing of a new filter. The 'fOrderBy'
  field is set to 'Key' and rest of the fields is set to 'Nothing'.
-}
complexFilter :: Filter
complexFilter = ComplexFilter { fOrderBy = Key
                              , fStartAt = Nothing
                              , fEndAt   = Nothing
                              , fEqualTo = Nothing
                              , fLimit   = Nothing
                              }
