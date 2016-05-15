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

data T3Field = T3BinaryField T3Sign ByteString deriving (Eq, Show)
data T3Record = T3Record T3Sign [T3Field] deriving (Eq, Show)
data T3File = T3File [T3Field] [T3Record] deriving (Eq, Show)

binaryField :: Get e ByteString
binaryField = getRemainingLazyByteString

fieldBody :: T3Sign -> Word32 -> Get e T3Field
fieldBody s _ = (T3BinaryField s) <$> binaryField

field :: Get String T3Field
field = do
  s <- sign `withError` "{0}: unexpected end of field"
  z <- size `withError` "{0}: unexpected end of field"
  isolate (fromIntegral z) (fieldBody s z) $ \c -> "{0}: field size mismatch: " ++ showHex z " expected, but " ++ show c ++ " consumed."

recordBody :: Get String [T3Field]
recordBody = whileM (not <$> isEmpty) field

recordTail :: Get (Either String ()) [T3Field]
recordTail = do
  z <- size `withError` Right ()
  gap
  onError Left $ isolate (fromIntegral z) recordBody $ \c -> "{0}: record size mismatch: " ++ showHex z " expected, but " ++ show c ++ " consumed."

record :: Get String T3Record
record = do
  s <- sign `withError` "{0}: unexpected end of record"
  t <- onError (either id (const "{0}: unexpected end of file")) recordTail
  return $ T3Record s t
  
fileSignature :: Get String ()
fileSignature = onError (const "File format not recognized.") $ expect (T3Mark TES3) sign

getT3File :: Get String T3File
getT3File = do
  fileSignature
  header <- onError (either id (const "{0}: unexpected end of file")) recordTail
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
