module Data.Tes3.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3

tests :: Test
tests = TestList
  [ TestCase t3StringTest
  , TestCase t3MarkValueTest
  , TestCase t3SignValueTest
  , TestCase t3MarkNewTest
  , TestCase t3SignNewTest
  , TestCase t3SignShowTest
  ]

t3StringTest :: Assertion
t3StringTest = do
  assertEqual "" (B.pack [192]) $ t3StringValue "А"
  assertEqual "" (B.pack [192, 225, 226, 32, 49, 51, 50]) $ t3StringValue "Абв 132"
  assertEqual "" "Абв 132" $ t3StringNew (B.pack [192, 225, 226, 32, 49, 51, 50])

t3MarkValueTest :: Assertion
t3MarkValueTest = do
  assertEqual "" 0x33534554 $ t3MarkValue TES3

t3SignValueTest :: Assertion
t3SignValueTest = do
  assertEqual "" 0x33534554 $ t3SignValue $ T3Mark TES3
  assertEqual "" 0x22222222 $ t3SignValue $ T3Sign 0x22222222

t3MarkNewTest :: Assertion
t3MarkNewTest = do
  assertEqual "" (Just TES3) $ t3MarkNew 0x33534554
  assertEqual "" Nothing $ t3MarkNew 0x33534553

t3SignNewTest :: Assertion
t3SignNewTest = do
  assertEqual "" (T3Mark TES3) $ t3SignNew 0x33534554
  assertEqual "" (T3Sign 0x33534553) $ t3SignNew 0x33534553

t3SignShowTest :: Assertion
t3SignShowTest = do
  assertEqual "" "FNAM" $ show $ T3Mark FNAM
  assertEqual "" "XXXX" $ show $ T3Sign 0x58585858
