--
-- Copyright 2016, 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

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
  assertEqual "" (B.pack [0 .. 255]) $ t3StringValue $ t3StringNew (B.pack [0 .. 255])

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
