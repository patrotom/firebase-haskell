import Test.Hspec

import qualified Firebase.Database.FiltersSpec
import qualified Firebase.Database.UtilsSpec
import qualified Firebase.Database.RequestsSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Firebase.Database.FiltersSpec" Firebase.Database.FiltersSpec.spec
  describe "Firebase.Database.UtilsSpec" Firebase.Database.UtilsSpec.spec
  describe "Firebase.Database.RequestsSpec" Firebase.Database.RequestsSpec.spec
