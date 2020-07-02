![HSpec](https://github.com/patrotom/firebase-haskell/workflows/CI/badge.svg)

# Firebase Haskell

A simple Haskell wrapper library for the [Firebase API](https://firebase.google.com/).

Currently implemented Firebase services:

* [Firebase Database](https://firebase.google.com/docs/database)

## Getting Started

This is a simple explanation of the basic functionalities of the library to get you on track fast.

### Firebase Database

First, this is a piece of boilerplate that should be in place before you try the examples:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Firebase.Database
import Data.Aeson


config = DbConfig { projectId = "your-project-id", authToken = Nothing }
```

Make a simple read **GET** request to fetch data from the root location of your project:

``` haskell
main :: IO ()
main = do
  let method = Read
      filter = EmptyFilter
      requestBody = EmptyBody
      location = ""
  response <- dbQuery method config location filter requestBody
  print response
```

Let's add some filters and nested location to this request. We take advantage of the prepared `complexFilter`:

``` haskell
main :: IO ()
main = do
  let method = Read
      filter = complexFilter { fOrderBy = Key
                             , fStartAt = Just $ Param (show "h")
                             }
      requestBody = EmptyBody
      location = "languages/functional"
  response <- dbQuery method config location filter requestBody
  print response
```

You can also make **PUT** (`Write`), **POST** (`Push`), and **PATCH** (`Update`) requests with a defined request body:

``` haskell
data Person = Person String Int
instance ToJSON Person where
    toJSON (Person name age) = object
        [ "name" .= name
        , "age"  .= age
        ]

main :: IO ()
main = do
  let method = Push
      filter = EmptyFilter
      requestBody = Body (Person "Thomas" 23)
      location = "people"
  response <- dbQuery method config location filter requestBody
  print response
```

You simply define the path and use **DELETE** (`Delete`) request to delete data at the location (path):

``` haskell
main :: IO ()
main = do
  let method = Delete
      filter = EmptyFilter
      requestBody = EmptyBody
      location = "dinosaurs/stegosaurus"
  response <- dbQuery method config location filter requestBody
  print response
```

## Running Tests

Since this is a [Stack](https://docs.haskellstack.org/en/stable/README/) project you simply run a `stack test` in the root of the repository.

## Bulding Documentation

Run `stack haddock` in the root of the repository to build the documentation.

## Contributing

Issues, bugs, and questions may be reported in [the GitHub issue tracker for this project](https://github.com/patrotom/firebase-haskell/issues).

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Authors

* **Tomáš Patro** - *Initial work*

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments

* This project was created at the [CTU, Faculty of Information Technology](https://fit.cvut.cz/) under the supervision of [Ing. Marek Suchánek](http://users.fit.cvut.cz/suchama4/).
