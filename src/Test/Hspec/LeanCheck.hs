-- |
-- Module      : Test.Hspec.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for the Hspec test framework.
--
-- Here's how your @spec.hs@ might look like:
--
-- > import Test.Hspec
-- > import Test.Hspec.LeanCheck as LC
-- >
-- > import Data.List (sort)
-- >
-- > main :: IO ()
-- > main = hspec spec
-- >
-- > spec :: Spec
-- > spec = do
-- >   describe "sort" $ do
-- >     it "is idempotent" $
-- >       LC.property $ \xs -> sort (sort xs :: [Int]) == sort xs
-- >     it "is identity" $ -- not really
-- >       LC.property $ \xs -> sort (xs :: [Int]) == xs
--
-- The output for the above program is:
--
-- > $ ./eg/minimal
-- >
-- > sort
-- >   is idempotent
-- >   is identity FAILED [1]
-- >
-- > Failures:
-- >
-- >   eg/minimal.hs:17:5:
-- >   1) sort is identity
-- >        [1,0]
-- >
-- >   To rerun use: --match "/sort/is identity/"
-- >
-- > 2 examples, 1 failure
--
-- Please see the documentation of
-- "Test.LeanCheck" and Hspec
-- for more details.
module Test.Hspec.LeanCheck
  ( property
  , propertyWith
  , module Test.LeanCheck
  )
where

import Test.Hspec.Core.Spec
import Test.LeanCheck
import Test.LeanCheck.Core (resultiers)
import Control.Exception (try)
import System.IO.Unsafe (unsafePerformIO) -- LeanCheck is pure
import qualified Test.HUnit.Lang as HUnit
import Data.Maybe (fromMaybe)

data Property = Ok
              | Failed String

-- TODO: catch errors
propertyWith :: Testable a => Int -> a -> Property
propertyWith m p = case counterExample m p of
  Nothing -> Ok
  Just ce -> Failed $ unwords ce

property :: Testable a => a -> Property
property = propertyWith 200

instance Example Property where
  evaluateExample p _ _ _ = return . Result ""
                          $ case p of
                            Ok -> Success
                            Failed s -> Failure Nothing (Reason s)

instance Testable (IO a) where
  resultiers action = unsafePerformIO $ do
    r <- try action
    return . (:[]) . (:[]) $ case r of
      Right _ -> ([],True)
      Left (HUnit.HUnitFailure loc reason) ->
        case reason of
        HUnit.Reason s -> (["--", s],False)
        HUnit.ExpectedButGot prefix expected actual ->
          ([fromMaybe "" prefix, "--", "expected", expected, "but got", actual], False)
