module Data.Tes3.Utils.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3.Utils

tests :: Test
tests = TestList
  [ TestCase writeRunTest
  , TestCase pRunTest
  , TestCase writeLinesTest
  , TestCase pLinesTest
  ]

writeRunTest :: Assertion
writeRunTest = do
  assertEqual "" "Qa\\ Bc\\ De" $ writeRun "Qa Bc De"
  assertEqual "" "Qa\\ Bc\\ De\\ " $ writeRun "Qa Bc De "
  assertEqual "" "Qa\\ Bc\\ \\rDe\\ " $ writeRun "Qa Bc \rDe "
  assertEqual "" "\\x07\\x07\\x07\\\\\\ " $ writeRun "\7\7\7\\ "

pRunTest :: Assertion
pRunTest = do
  assertEqual "" (Right "Qa Bc De") $ Tp.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ De"
  assertEqual "" (Right "Qa Bc De ") $ Tp.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ De\\ "
  assertEqual "" (Right "Qa Bc \rDe ") $ Tp.parseOnly (pRun <* Tp.endOfInput) "Qa\\ Bc\\ \\rDe\\ "
  assertEqual "" (Right "\7\7\7\\ ") $ Tp.parseOnly (pRun <* Tp.endOfInput) "\\x07\\x07\\x07\\\\\\ "

writeLinesTest :: Assertion
writeLinesTest = do
  assertEqual "" "    Qa Bc\n     De\n    \n    \n" $ writeLines ["Qa Bc", " De", "", ""]

pLinesTest :: Assertion
pLinesTest = do
  assertEqual "" (Right ["Qa Bc", " De", "", ""]) $ Tp.parseOnly (pLines <* Tp.endOfInput) "    Qa Bc\r\n     De\n    \n    \n"
