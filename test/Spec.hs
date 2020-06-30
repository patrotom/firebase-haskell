import Test.Hspec

import qualified Firebase.DatabaseSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Firebase.DatabaseSpec" Firebase.DatabaseSpec.spec
