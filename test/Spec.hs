import Test.Hspec

import qualified Firebase.Database.FiltersSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Firebase.Database.FiltersSpec" Firebase.Database.FiltersSpec.spec
