-- |
-- Module      : Test.Hspec.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for the Hspec test framework.
--
-- Here's how your @test.hs@ might look like:
--
-- > TODO: TBA
--
-- The output for the above program is:
--
-- > TODO: TBA
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

data Property = Ok
              | Failed String

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
