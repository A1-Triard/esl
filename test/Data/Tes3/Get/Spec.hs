module Data.Tes3.Get.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Tes3
import Data.Tes3.Get

tests :: Test
tests = TestList
  [ TestCase parseEmptyFile
  , TestCase parseShortInvalidFile
  , TestCase parseLongInvalidFile
  , TestCase parseFileWithValidSignature
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

testFile1Bytes :: ByteString
testFile1Bytes
  =  C.pack "TES3"
  <> w32 346
  <> w64 0
  <> C.pack "HEDR"
  <> w32 300
  <> w32 0x07
  <> w32 32
  <> B.fromStrict testAuthor
  <> B.fromStrict testDescription
  <> w32 39
  <> C.pack "MAST"
  <> w32 14
  <> C.pack "Morrowind.esm\0"
  <> C.pack "DATA"
  <> w32 8
  <> w64 137
  <> C.pack "CLOH"
  <> w32 29
  <> w64 0
  <> C.pack "NAMF"
  <> w32 8
  <> C.pack "namename"
  <> C.pack "IDID"
  <> w32 5
  <> C.pack "idid\0"

testAuthor :: S.ByteString
testAuthor = SC.pack $ replace "0" "\0"
  (  "test author00000"
  ++ "0000000000000000"
  )

testDescription :: S.ByteString
testDescription = SC.pack $ replace "0" "\0" $ replace "\n" "\r\n"
  (  "test description"
  ++ "\nAAA\n000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  ++ "0000000000000000"
  )

testFile1 :: T3File
testFile1 = T3File
  ( T3Header 0x07 (KnownT3FileType ESS) "test author" ["test description", "AAA", ""] 39
    [ T3FileRef "Morrowind.esm\0" 137
    ]
  )
  [ T3Record (read "CLOH") 0
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

parseFileWithValidSignature :: Assertion
parseFileWithValidSignature = do
  assertEqual "" (Left "10h: unexpected end of header") $ runGetT3File $ C.pack "TES3" <> w32 0 <> w64 0

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (Right testFile1) $ runGetT3File testFile1Bytes
