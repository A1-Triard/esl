module Data.Tes3.Parser.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3
import Data.Tes3.Parser

tests :: Test
tests = TestList
  [ TestCase parseValidFile
  , TestCase parseInvalidFile
  ]

testFile1Text :: S.Text
testFile1Text
  =  "3SET\n"
  <> "VERSION 1067869798\n"
  <> "TYPE ESP\n"
  <> "AUTHOR Ath\n"
  <> "DESCRIPTION\n"
  <> "    Descr descr descr.\n"
  <> "Morrowind.esm 79764287\n"
  <> "\n"
  <> "CLOT\n"
  <> "NAME _ale_leather_skirt\n"
  <> "MODL Aleanne\\\\dr_a_fC_la_25_gnd.nif\n"
  <> "FNAM Длинная тога\n"
  <> "CTDT AgAAAAAAQEB4AFgC\n"
  <> "CNAM _ale_dr_a_fC_1_025%\n"
  <> "\n"
  <> "CLOT 51\n"
  <> "NAME _ale_short_toga\n"
  <> "MODL Aleanne\\\\dr_b_fC_la_20s_gnd.nif\n"
  <> "FNAM Короткая тога\n"
  <> "CTDT AgAAAAAAgD94AFgC\n"
  <> "CNAM _ale_dr_b_fC_la_20s%\n"

invalidTestFileText :: S.Text
invalidTestFileText
  =  "3SET\n"
  <> "VERSION 1067869798\n"
  <> "TYPE ESP\n"
  <> "AUTHOR Ath\n"
  <> "DESCRIPTION\n"
  <> "   Descr descr descr.\n"
  <> "Morrowind.esm 79764287\n"
  <> "\n"
  <> "CLOT\n"
  <> "NAME _ale_leather_skirt\n"
  <> "MODL Aleanne\\\\dr_a_fC_la_25_gnd.nif\n"
  <> "FNAM Длинная тога\n"
  <> "CTDT AgAAAAAAQEB4AFgC\n"
  <> "CNAM _ale_dr_a_fC_1_025%\n"
  <> "\n"
  <> "CLOT 51\n"
  <> "NAME _ale_short_toga\n"
  <> "MODL Aleanne\\\\dr_b_fC_la_20s_gnd.nif\n"
  <> "FNAM Короткая тога\n"
  <> "CTDT AgAAAAAAgD94AFgC\n"
  <> "CNAM _ale_dr_b_fC_la_20s%\n"

data T3File = T3File T3FileHeader [T3Record] deriving (Eq, Show)

testFile1 :: T3File
testFile1 = T3File
  ( T3FileHeader 1067869798 (KnownT3FileType ESP) "Ath" ["Descr descr descr."]
    [ T3FileRef "Morrowind.esm\0" 79764287
    ]
  )
  [ T3Record (T3Mark CLOT) 0
    [ T3StringField (T3Mark NAME) "_ale_leather_skirt\0"
    , T3StringField (T3Mark MODL) "Aleanne\\dr_a_fC_la_25_gnd.nif\0"
    , T3StringField (T3Mark FNAM) "Длинная тога\0"
    , T3BinaryField (T3Mark CTDT) "\STX\NUL\NUL\NUL\NUL\NUL@@x\NULX\STX"
    , T3StringField (T3Mark CNAM) "_ale_dr_a_fC_1_025"
    ]
  , T3Record (T3Mark CLOT) 51
    [ T3StringField (T3Mark NAME) "_ale_short_toga\0"
    , T3StringField (T3Mark MODL) "Aleanne\\dr_b_fC_la_20s_gnd.nif\0"
    , T3StringField (T3Mark FNAM) "Короткая тога\0"
    , T3BinaryField (T3Mark CTDT) "\STX\NUL\NUL\NUL\NUL\NUL\128?x\NULX\STX"
    , T3StringField (T3Mark CNAM) "_ale_dr_b_fC_la_20s"
    ]
  ]

pT3File :: T.Parser T3File
pT3File = do
  pT3FileSignature
  h <- pT3FileHeader
  r <- many pT3Record
  return $ T3File h r

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (Right testFile1) $ TP.parseOnly (pT3File <* Tp.endOfInput) testFile1Text

parseInvalidFile :: Assertion
parseInvalidFile = do
  assertEqual "" (Left "endOfInput") $ TP.parseOnly (pT3File <* Tp.endOfInput) invalidTestFileText
