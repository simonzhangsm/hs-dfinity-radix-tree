import Test.Tasty

import qualified UnitTests
import qualified Properties

main :: IO ()
main = do
    unit_tests <- UnitTests.tests
    defaultMain $ testGroup "tests" [unit_tests, Properties.tests]
