import ClassyPrelude

import Test.Tasty
import qualified MIME.Parser.Test as MP
import Test.Tasty.QuickCheck
import Mail (Target)

tests :: TestTree
tests = testGroup "Tests"
  [ MP.tests
  ]

main :: IO ()
main = do
  {-sample $ (MP.arbitrary :: Gen Target)-}
  defaultMain tests
