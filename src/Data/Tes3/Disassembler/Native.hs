module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
  
labelEOF :: String -> Get a -> Get a
labelEOF s = label $ showString "REPLACE/not enough bytes/" s

labelError :: String -> Get a -> Get a
labelError s = label $ showString "ERROR/" s

getError :: ByteOffset -> String -> String
getError offset s =
  replace "BYTES" (showHex offset "") $ process $ viewL $ lines s
  where
    process Nothing = ""
    process (Just (e, rules)) = foldl apply e rules
    apply e ('R' : 'E' : 'P' : 'L' : 'A' : 'C' : 'E' : '/' : r) =
      case splitOn "/" r of
        (a : b : []) -> if e == a then b else e
        _ -> e
    apply _ ('E' : 'R' : 'R' : 'O' : 'R' : '/' : r) = r
    apply e _  = e

expect :: (Show a, Eq a) => a -> Get a -> Get ()
expect expected getter = do
  offset <- bytesRead
  actual <- getter
  if actual /= expected
    then fail $ showHex offset $ showString ": " $ shows expected $ showString " expected, but " $ shows actual " provided."
    else return ()

sign :: Get T3Sign
sign = t3SignNew <$> getWord32le

size :: Get Word32
size = getWord32le

gap :: Get ()
gap = expect 0 getWord64le

data T3Field = T3BinaryField T3Sign ByteString
data T3Record = T3Record T3Sign [T3Field]
data T3File = T3File [T3Field] [T3Record]

binaryField :: Get ByteString
binaryField = getRemainingLazyByteString

fieldBody :: T3Sign -> Word32 -> Get T3Field
fieldBody s _ = (T3BinaryField s) <$> binaryField

field :: Get T3Field
field = do
  s <- sign
  z <- size
  isolate (fromIntegral z) $ fieldBody s z

recordBody :: Get [T3Field]
recordBody = whileM (not <$> isEmpty) field

recordTail :: Get [T3Field]
recordTail = do
  z <- size
  gap
  isolate (fromIntegral z) recordBody
  
record :: Get T3Record
record = do
  s <- sign
  t <- recordTail
  return $ T3Record s t
  
fileSignature :: Get ()
fileSignature = labelError "File format not recognized." $ expect (T3Mark TES3) sign

file :: Get T3File
file = labelEOF "BYTES: unexpected end of file." $ do
  fileSignature
  header <- recordTail
  records <- whileM (not <$> isEmpty) record
  return $ T3File header records

parseFile :: ByteString -> Either String T3File
parseFile b =
  case pushEndOfInput $ runGetIncremental file `pushChunks` b of
    Fail _ offset e -> Left $ getError offset e
    Done (SB.null -> False) offset _ -> Left $ showHex offset ": end of file expected."
    Done _ _ f -> Right f
    _ -> Left "Internal error."

disassembly :: ByteString -> Either String String
disassembly b = do
  _ <- parseFile b
  return "ESP\n"
