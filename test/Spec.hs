#define TESTS
#include <haskell>
import qualified Data.Tes3.Utils.Spec
import qualified Data.Tes3.Spec
import qualified Data.Tes3.Get.Spec
import qualified Data.Tes3.Parser.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.Tes3.Utils.Spec.tests
  , Data.Tes3.Spec.tests
  , Data.Tes3.Get.Spec.tests
  , Data.Tes3.Parser.Spec.tests
  ]
