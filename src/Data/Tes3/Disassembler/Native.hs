module Data.Tes3.Disassembler.Native where

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

gap :: Get (Either String ()) ()
gap = expect 0 getWord64le

binaryField :: Get e ByteString
binaryField = getRemainingLazyByteString

fieldBody :: T3Sign -> Word32 -> Get e T3Field
fieldBody s _ = (T3BinaryField s) <$> binaryField

field :: Get String T3Field
field = do
  s <- sign `withError` "{0}: unexpected end of field"
  z <- size `withError` "{0}: unexpected end of field"
  isolate (fromIntegral z) (fieldBody s z) $ \c -> "{0}: field size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

recordBody :: Get String [T3Field]
recordBody = whileM (not <$> isEmpty) field

recordTail :: Get (Either String ()) [T3Field]
recordTail = do
  z <- size `withError` Right ()
  gap
  onError Left $ isolate (fromIntegral z) recordBody $ \c -> "{0}: record size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

record :: Get String T3Record
record = do
  s <- sign `withError` "{0}: unexpected end of record"
  t <- onError (either id (const "{0}: unexpected end of record")) recordTail
  return $ T3Record s t
  
fileSignature :: Get String ()
fileSignature = onError (const "File format not recognized.") $ expect (T3Mark TES3) sign

fileRef :: Get (Either String ()) T3FileRef
fileRef = do
  expect (T3Mark MAST) sign
  m <- size `withError` Right ()
  name <- getByteString (fromIntegral m) `withError` Right ()
  expect (T3Mark DATA) sign
  expect 8 size
  z <- getWord64le `withError` Right ()
  return $ T3FileRef name z

fileHeaderData :: Get (Either String ()) T3Header
fileHeaderData = do
  expect (T3Mark HEDR) sign
  expect 300 size
  version <- getWord32le `withError` Right ()
  file_type <- t3FileTypeNew <$> getWord32le `withError` Right ()
  author <- getByteString 32 `withError` Right ()
  description <- getByteString 256 `withError` Right ()
  items_count <- getWord32le `withError` Right ()
  refs <- whileM (not <$> isEmpty) fileRef
  return $ T3Header version file_type author description items_count refs

fileHeader :: Get (Either String ()) T3Header
fileHeader = do
  z <- size `withError` Right ()
  gap
  let t = onError (either id (const "{0}: unexpected end of header")) fileHeaderData
  onError Left $ isolate (fromIntegral z) t $ \c -> "{0}: header size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

getT3File :: Get String T3File
getT3File = do
  fileSignature
  header <- onError (either id (const "{0}: unexpected end of file")) fileHeader
  records <- whileM (not <$> isEmpty) record
  return $ T3File header records

runGetT3File :: ByteString -> Either String T3File
runGetT3File b =
  case pushEndOfInput $ runGetIncremental 0 getT3File `pushChunks` b of
    Fail _ offset e -> Left $ replace "{0}" (showHex offset "h") $ either ("Internal error: " ++) id e
    Done (SB.null -> False) offset _ -> Left $ showHex offset "h: end of file expected."
    Done _ _ f -> Right f
    _ -> Left "Internal error."

disassembly :: ByteString -> Either String String
disassembly b = do
  _ <- runGetT3File b
  return "ESP\n"
