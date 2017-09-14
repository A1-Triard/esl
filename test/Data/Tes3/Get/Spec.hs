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
  , TestCase parseAdjustableFile
  , TestCase parseFileWithInvalidFlags
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

testFileWithInvalidFlagsBytes :: ByteString
testFileWithInvalidFlagsBytes
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
  <> w64 137
  <> C.pack "NAMF"
  <> w32 8
  <> C.pack "namename"
  <> C.pack "IDID"
  <> w32 5
  <> C.pack "idid\0"

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

testFile2Bytes :: ByteString
testFile2Bytes
  =  C.pack "TES3"
  <> w32 346
  <> w64 0
  <> C.pack "HEDR"
  <> w32 300
  <> w32 0x07
  <> w32 32
  <> B.fromStrict testAuthor
  <> B.fromStrict testDescription
  <> w32 2
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
  <> C.pack "SCPT"
  <> w32 22
  <> w64 0
  <> C.pack "SCTX"
  <> w32 14
  <> C.pack "script\0\r\ntext\0"

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

sign :: S.Text -> T3Sign
sign t =
  case ST.unpack t of
    [a, b, c, d] -> t3SignNew $ fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)
    _ -> error "sign"

fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fromBytes a b c d
  =  (fromIntegral a)
  .|. (shift (fromIntegral b) 8)
  .|. (shift (fromIntegral c) 16)
  .|. (shift (fromIntegral d) 24)

testFile1 :: [T3Record]
testFile1 =
  [ T3Record (sign "TES3") t3FlagsEmpty
    [ T3HeaderField (sign "HEDR") (T3FileHeader 0x07 ESS "test author" ["test description", "AAA", ""] 1)
    , T3StringField (sign "MAST") "Morrowind.esm\0"
    , T3LongField (sign "DATA") 137
    ]
  , T3Record (sign "CLOH") t3FlagsEmpty
    [ T3BinaryField (sign "NAMF") "namename"
    , T3BinaryField (sign "IDID") "idid\0"
    ]
  ]

testFile2 :: [T3Record]
testFile2 =
  [ T3Record (sign "TES3") t3FlagsEmpty
    [ T3HeaderField (sign "HEDR") (T3FileHeader 0x07 ESS "test author" ["test description", "AAA", ""] 2)
    , T3StringField (sign "MAST") "Morrowind.esm\0"
    , T3LongField (sign "DATA") 137
    ]
  , T3Record (sign "CLOH") t3FlagsEmpty
    [ T3BinaryField (sign "NAMF") "namename"
    , T3BinaryField (sign "IDID") "idid\0"
    ]
  , T3Record (sign "SCPT") t3FlagsEmpty
    [ T3MultilineField (sign "SCTX") ["script\0", "text"]
    ]
  ]

getT3File :: Bool -> Get (ByteOffset -> String) [T3Record]
getT3File adjust = do
  getT3FileSignature
  first_record <- getT3FirstRecord adjust
  let T3Record _ flags fields = first_record
  if flags /= t3FlagsEmpty
    then failG $ const $ "Invlaid file flags."
    else
      case fields of
        (T3HeaderField _ (T3FileHeader _ _ _ _ items_count ): _) -> do
          records <- whileM (not <$> isEmpty) $ getT3Record adjust
          if fromIntegral items_count /= length records
            then failG $ const $ "Records count mismatch: " ++ show items_count ++ " expected, but " ++ show (length records) ++ " readed."
            else return (first_record : records)
        _ -> failG $ const $ "Invalid file header."

runGetT3File :: Bool -> ByteString -> (ByteOffset, Either String [T3Record])
runGetT3File adjust inp =
  case pushEndOfInput $ runGetIncremental 0 (getT3File adjust) `pushChunks` inp of
    G.Done (SB.null -> True) offset r -> (offset, Right r)
    G.Fail _ offset (Right e) -> (offset, Left $ e offset)
    _ -> error "runGetT3File"

parseEmptyFile :: Assertion
parseEmptyFile = do
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File False B.empty

parseShortInvalidFile :: Assertion
parseShortInvalidFile = do
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File False $ C.pack "TE"
  assertEqual "" (0, Left "File format not recognized.") $ runGetT3File False $ C.pack "X0"

parseLongInvalidFile :: Assertion
parseLongInvalidFile = do
  assertEqual "" (4, Left "File format not recognized.") $ runGetT3File False $ C.pack "TEhfdskj fsd jhfg gjf jhs"
  assertEqual "" (4, Left "File format not recognized.") $ runGetT3File False $ C.pack "X0 fhsdm hfsdg jhfsdg fjs gd"

parseFileWithValidSignature :: Assertion
parseFileWithValidSignature = do
  assertEqual "" (16, Left "Invalid file header.") $ runGetT3File False $ C.pack "TES3" <> w32 0 <> w64 0

parseFileWithInvalidItemsCount :: Assertion
parseFileWithInvalidItemsCount = do
  assertEqual "" (407, Left "Records count mismatch: 39 expected, but 1 readed.") $ runGetT3File False testFileWithInvalidItemsCountBytes

parseValidFile :: Assertion
parseValidFile = do
  assertEqual "" (407, Right testFile1) $ runGetT3File False testFile1Bytes

parseAdjustableFile :: Assertion
parseAdjustableFile = do
  assertEqual "" (445, Right testFile2) $ runGetT3File True testFile2Bytes

parseFileWithInvalidFlags :: Assertion
parseFileWithInvalidFlags = do
  assertEqual "" (378, Left "172h: invalid record flags (89h).") $ runGetT3File False testFileWithInvalidFlagsBytes
