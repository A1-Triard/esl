module Data.Tes3.Get.Native where

#include <haskell>
import Data.Tes3

expect :: (Show a, Eq a) => a -> Get e a -> Get (Either String e) ()
expect expected getter = do
  offset <- totalBytesRead
  actual <- onError Right getter
  if actual /= expected
    then failG $ Left $ concat [showHex offset $ "h: ", shows expected " expected, but ", shows actual " provided."]
    else return ()

sign :: Get () T3Sign
sign = t3SignNew <$> getWord32le

size :: Get () Word32
size = getWord32le

gap :: Get () Word64
gap = getWord64le

binaryField :: Get e ByteString
binaryField = getRemainingLazyByteString

stringField :: Get e String
stringField = t3StringNew <$> getRemainingLazyByteString

multilineField :: Get e [String]
multilineField = splitOn "\r\n" <$> t3StringNew <$> getRemainingLazyByteString

refField :: Get () (Word32, String)
refField = do
  z <- getWord32le
  n <- getLazyByteString 32
  return (z, trimNulls $ t3StringNew n)

fieldBody :: T3Sign -> Get () T3Field
fieldBody s
  | t3FieldType s == T3String = T3StringField s <$> stringField
  | t3FieldType s == T3Multiline = T3MultilineField s <$> multilineField
  | t3FieldType s == T3Ref = (\(z, n) -> T3RefField s z n) <$> refField
  | otherwise = T3BinaryField s <$> binaryField

field :: Get String T3Field
field = do
  s <- sign `withError` "{0}: unexpected end of field"
  z <- size `withError` "{0}: unexpected end of field"
  let body = fieldBody s `withError` "{0}: unexpected end of field"
  isolate (fromIntegral z) body $ \c -> "{0}: field size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

recordBody :: Get String [T3Field]
recordBody = whileM (not <$> isEmpty) field

recordTail :: Get (Either String ()) (Word64, [T3Field])
recordTail = do
  z <- size `withError` Right ()
  g <- gap `withError` Right ()
  f <- onError Left $ isolate (fromIntegral z) recordBody $ \c -> "{0}: record size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."
  return (g, f)

record :: Get String T3Record
record = do
  s <- sign `withError` "{0}: unexpected end of record"
  (g, f) <- onError (either id (const "{0}: unexpected end of record")) recordTail
  return $ T3Record s g f
  
fileSignature :: Get String ()
fileSignature = onError (const "File format not recognized.") $ expect (T3Mark TES3) sign

fileRef :: Get (Either String ()) T3FileRef
fileRef = do
  expect (T3Mark MAST) sign
  m <- size `withError` Right ()
  name <- t3StringNew <$> B.fromStrict <$> getByteString (fromIntegral m) `withError` Right ()
  expect (T3Mark DATA) sign
  expect 8 size
  z <- getWord64le `withError` Right ()
  return $ T3FileRef name z

trimNulls :: String -> String
trimNulls = reverse . dropWhile (== '\0') . reverse

fileHeaderData :: Get (Either String ()) (Word32, T3Header)
fileHeaderData = do
  expect (T3Mark HEDR) sign
  expect 300 size
  version <- getWord32le `withError` Right ()
  file_type <- t3FileTypeNew <$> getWord32le `withError` Right ()
  author <- trimNulls <$> t3StringNew <$> B.fromStrict <$> getByteString 32 `withError` Right ()
  description <- splitOn "\r\n" <$> trimNulls <$> t3StringNew <$> B.fromStrict <$> getByteString 256 `withError` Right ()
  items_count <- getWord32le `withError` Right ()
  refs <- whileM (not <$> isEmpty) fileRef
  return (items_count, T3Header version file_type author description refs)

fileHeader :: Get (Either String ()) (Word32, T3Header)
fileHeader = do
  z <- size `withError` Right ()
  expect 0 gap
  let t = onError (either id (const "{0}: unexpected end of header")) fileHeaderData
  onError Left $ isolate (fromIntegral z) t $ \c -> "{0}: header size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

getT3File :: Get String T3File
getT3File = do
  fileSignature
  (items_count, header) <- onError (either id (const "{0}: unexpected end of file")) fileHeader
  records <- whileM (not <$> isEmpty) record
  let actual_items_count = length records
  if actual_items_count /= fromIntegral items_count
    then failG $ "Records count mismatch: " ++ show items_count ++ " expected, but " ++ show actual_items_count ++ " readed."
    else return $ T3File header records

runGetT3File :: ByteString -> Either String T3File
runGetT3File b =
  case pushEndOfInput $ runGetIncremental 0 getT3File `pushChunks` b of
    Fail _ offset e -> Left $ replace "{0}" (showHex offset "h") $ either ("Internal error: " ++) id e
    Done (SB.null -> False) offset _ -> Left $ showHex offset "h: end of file expected."
    Done _ _ f -> Right f
    _ -> Left "Internal error."
