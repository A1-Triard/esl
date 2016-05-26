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

stringField :: Get e Text
stringField = t3StringNew <$> getRemainingLazyByteString

multilineField :: Get e [Text]
multilineField = T.splitOn "\r\n" <$> t3StringNew <$> getRemainingLazyByteString

multiStringField :: Get e [Text]
multiStringField = T.splitOn "\0" <$> t3StringNew <$> getRemainingLazyByteString

refField :: Get () (Word32, Text)
refField = do
  z <- getWord32le
  n <- getLazyByteString 32
  return (z, T.dropWhileEnd (== '\0') $ t3StringNew n)

fixedStringField :: Word32 -> Get () Text
fixedStringField z = T.dropWhileEnd (== '\0') <$> t3StringNew <$> getLazyByteString (fromIntegral z)

floatField :: Get () Float
floatField = wordToFloat <$> getWord32le

fieldBody :: T3Sign -> T3Sign -> Get () T3Field
fieldBody record_sign s =
  f (t3FieldType record_sign s)
  where
    f (T3FixedString z) = T3FixedStringField s <$> fixedStringField z
    f T3String = T3StringField s <$> stringField
    f T3Multiline = T3MultilineField s <$> multilineField
    f T3MultiString = T3MultiStringField s <$> multiStringField
    f T3Ref = (\(z, n) -> T3RefField s z n) <$> refField
    f T3Binary = T3BinaryField s <$> binaryField
    f T3Float = T3FloatField s <$> floatField

field :: T3Sign -> Get String T3Field
field record_sign = do
  s <- sign `withError` "{0}: unexpected end of field"
  z <- size `withError` "{0}: unexpected end of field"
  let body = fieldBody record_sign s `withError` "{0}: unexpected end of field"
  isolate (fromIntegral z) body $ \c -> "{0}: field size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

recordBody :: T3Sign -> Get String [T3Field]
recordBody s = whileM (not <$> isEmpty) $ field s

recordTail :: T3Sign -> Get (Either String ()) (Word64, [T3Field])
recordTail s = do
  z <- size `withError` Right ()
  g <- gap `withError` Right ()
  f <- onError Left $ isolate (fromIntegral z) (recordBody s) $ \c -> "{0}: record size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."
  return (g, f)

getT3Record :: Get String T3Record
getT3Record = do
  s <- sign `withError` "{0}: unexpected end of record"
  (g, f) <- onError (either id (const "{0}: unexpected end of record")) $ recordTail s
  return $ T3Record s g f

getT3FileSignature :: Get String ()
getT3FileSignature = onError (const "File format not recognized.") $ expect (T3Mark TES3) sign

fileRef :: Get (Either String ()) T3FileRef
fileRef = do
  expect (T3Mark MAST) sign
  m <- size `withError` Right ()
  name <- t3StringNew <$> B.fromStrict <$> getByteString (fromIntegral m) `withError` Right ()
  expect (T3Mark DATA) sign
  expect 8 size
  z <- getWord64le `withError` Right ()
  return $ T3FileRef name z

fileHeaderData :: Get (Either String ()) (T3FileHeader, Word32)
fileHeaderData = do
  expect (T3Mark HEDR) sign
  expect 300 size
  version <- getWord32le `withError` Right ()
  file_type_value <- getWord32le `withError` Right ()
  file_type <- case t3FileTypeNew file_type_value of
    Nothing -> failG $ Left $ "Unknown file type: " ++ show file_type_value
    Just x -> return x
  author <- T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 32 `withError` Right ()
  description <- T.splitOn "\r\n" <$> T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 256 `withError` Right ()
  items_count <- getWord32le `withError` Right ()
  refs <- whileM (not <$> isEmpty) fileRef
  return (T3FileHeader version file_type author description refs, items_count)

fileHeader :: Get (Either String ()) (T3FileHeader, Word32)
fileHeader = do
  z <- size `withError` Right ()
  expect 0 gap
  let t = onError (either id (const "{0}: unexpected end of header")) fileHeaderData
  onError Left $ isolate (fromIntegral z) t $ \c -> "{0}: header size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

getT3FileHeader :: Get String (T3FileHeader, Word32)
getT3FileHeader = onError (either id (const "{0}: unexpected end of file")) fileHeader
