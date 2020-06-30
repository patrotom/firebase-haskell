{-# LANGUAGE OverloadedStrings #-}

module Firebase.Database.DatabaseSpec (spec) where

import Test.Hspec
import Network.HTTP.Req
import Firebase.Database
import Firebase.Database.Types
import qualified Network.HTTP.Client as L


spec :: Spec
spec = do
  describe "filterParams" $ do
    it "works with empty filter" $
      filterParams EmptyFilter `shouldBe` (mempty :: Option s)
    it "works with shallow filter" $
      filterParams Shallow `shouldBe` (("shallow" =: True) :: Option s)


req_ :: (MonadHttp m,
         HttpMethod method,
         HttpBody body,
         HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
         method -> Url scheme -> body -> Option scheme -> m L.Request
req_ method url' body options =
  req' method url' body options $ \request _ -> return request