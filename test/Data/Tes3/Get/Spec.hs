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
  , TestCase parseFileWithInvalidItemsCount
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
  <> w32 1
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

testFileWithInvalidItemsCountBytes :: ByteString
testFileWithInvalidItemsCountBytes
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

data T3File = T3File T3FileHeader [T3Record] deriving (Eq, Show)

sign :: S.Text -> T3Sign
sign t =
  case Tp.parseOnly (pT3Sign <* Tp.endOfInput) t of
    Left e -> error e
    Right r -> r

testFile1 :: T3File
testFile1 = T3File
  ( T3FileHeader 0x07 (KnownT3FileType ESS) "test author" ["test description", "AAA", ""]
    [ T3FileRef "Morrowind.esm\0" 137
    ]
  )
  [ T3Record (sign "CLOH") 0
    [ T3BinaryField (sign "NAMF") "namename"
    , T3BinaryField (sign "IDID") "idid\0"
    ]
  ]

getT3File :: Get String T3File
getT3File = do
  getT3FileSignature
  (h, items_count) <- getT3FileHeader
  records <- whileM (not <$> isEmpty) getT3Record
  if fromIntegral items_count /= length records
    then failG $ "Records count mismatch: " ++ show items_count ++ " expected, but " ++ show (length records) ++ " readed."
    else return $ T3File h records

runGetT3File :: ByteString -> (ByteOffset, Either String T3File)
runGetT3File inp =
  case pushEndOfInput $ runGetIncremental 0 getT3File `pushChunks` inp of
    Done (SB.null -> True) offset r -> (offset, Right r)
    Fail _ offset (Right e) -> (offset, Left e)
    _ -> error "runGetT3File"

parseEmptyFile :: Assertion
parseEmptyFile = do
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File B.empty

parseShortInvalidFile :: Assertion
parseShortInvalidFile = do
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File $ C.pack "TE"
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File $ C.pack "X0"

parseLongInvalidFile :: Assertion
parseLongInvalidFile = do
  assertEqual "" (4, Left "File format not recognized.") $ runGetT3File $ C.pack "TEhfdskj fsd jhfg gjf jhs"
  assertEqual "" (4, Left "File format not recognized.") $ runGetT3File $ C.pack "X0 fhsdm hfsdg jhfsdg fjs gd"

parseFileWithValidSignature :: Assertion
parseFileWithValidSignature = do
  assertEqual "" (16, Left "{0}: unexpected end of header") $ runGetT3File $ C.pack "TES3" <> w32 0 <> w64 0

parseFileWithInvalidItemsCount :: Assertion
parseFileWithInvalidItemsCount = do
  assertEqual "" (407, Left "Records count mismatch: 39 expected, but 1 readed.") $ runGetT3File testFileWithInvalidItemsCountBytes

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (407, Right testFile1) $ runGetT3File testFile1Bytes
