module Data.Tes3.Disassembler.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3
import Data.Tes3.Disassembler

tests :: Test
tests = TestList
  [ TestCase parseEmptyFile
  , TestCase parseShortInvalidFile
  , TestCase parseLongInvalidFile
  , TestCase parseValidFile
  ]

leBytes :: (Integral a, FiniteBits a) => a -> ByteString
leBytes w =
  B.pack $ take (finiteBitSize w `div` 8) $ unfoldr go w
  where
    go n = Just (fromIntegral $ n .&. 0xFF, shift n (-8))

w32 :: Word32 -> ByteString
w32 = leBytes

w64 :: Word64 -> ByteString
w64 = leBytes

testFile0Bytes :: ByteString
testFile0Bytes
  =  C.pack "TES3"
  <> w32 0
  <> w64 0

testFile0 :: T3File
testFile0 = T3File [] []

testFile1Bytes :: ByteString
testFile1Bytes
  =  C.pack "TES3"
  <> w32 9
  <> w64 0
  <> C.pack "TEDR"
  <> w32 1
  <> C.pack "h"
  <> C.pack "CLOH"
  <> w32 29
  <> w64 0
  <> C.pack "NAMF"
  <> w32 8
  <> C.pack "namename"
  <> C.pack "IDID"
  <> w32 5
  <> C.pack "idid\0"

testFile1 :: T3File
testFile1 = T3File
  [ T3BinaryField (read "TEDR") "h"
  ]
  [ T3Record (read "CLOH")
    [ T3BinaryField (read "NAMF") "namename"
    , T3BinaryField (read "IDID") "idid\0"
    ]
  ]

parseEmptyFile :: Assertion
parseEmptyFile = do
  assertEqual "" (Left "File format not recognized.") $ runGetT3File B.empty

parseShortInvalidFile :: Assertion
parseShortInvalidFile = do
  assertEqual "" (Left "File format not recognized.") $ runGetT3File $ C.pack "TE"
  assertEqual "" (Left "File format not recognized.") $ runGetT3File $ C.pack "X0"

parseLongInvalidFile :: Assertion
parseLongInvalidFile = do
  assertEqual "" (Left "File format not recognized.") $ runGetT3File $ C.pack "TEhfdskj fsd jhfg gjf jhs"
  assertEqual "" (Left "File format not recognized.") $ runGetT3File $ C.pack "X0 fhsdm hfsdg jhfsdg fjs gd"

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (Right testFile0) $ runGetT3File testFile0Bytes
  assertEqual "" (Right testFile1 ) $ runGetT3File testFile1Bytes
