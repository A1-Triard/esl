module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3

newtype GetEx a = GetEx (ByteOffset -> Get a)
instance Monad GetEx where
  GetEx x >>= y =
    GetEx result
    where
      result base_offset = do
        p <- x base_offset
        let GetEx z = y p
        z base_offset
  fail = ex . fail
        
instance Functor GetEx where
  fmap f (GetEx x) =
    GetEx result
    where
      result base_offset = f <$> x base_offset

instance Applicative GetEx where
  pure a = GetEx $ const $ return a
  GetEx f <*> GetEx x =
    GetEx result
    where
      result base_offset = do
        ff <- f base_offset
        xx <- x base_offset
        return $ ff xx

instance Alternative GetEx where
  empty = ex $ empty
  GetEx a <|> GetEx b =
    GetEx result
    where
      result base_offset = a base_offset <|> b base_offset

deriving instance MonadPlus GetEx

bytesReadEx :: GetEx ByteOffset
bytesReadEx =
  GetEx result
  where
    result base_offset = do
      b <- bytesRead
      return $ base_offset + b

ex :: Get a -> GetEx a
ex getter = GetEx $ const getter

labelEx :: String -> GetEx a -> GetEx a
labelEx s (GetEx getter) =
  GetEx result
  where
    result base_offset = label s $ getter base_offset

isolateEx :: Int -> GetEx a -> GetEx a
isolateEx n (GetEx getter) =
  GetEx result
  where
    result base_offset = do
      offset <- bytesRead
      isolate n $ getter (base_offset + offset)

isEmptyEx :: GetEx Bool
isEmptyEx = ex isEmpty

runEx :: ByteOffset -> GetEx a -> Get a
runEx base_offset (GetEx getter) = getter base_offset

labelEOF :: String -> GetEx a -> GetEx a
labelEOF s = labelEx $ showString "REPLACE/not enough bytes/" s

labelError :: String -> GetEx a -> GetEx a
labelError s = labelEx $ showString "ERROR/" s

getError :: ByteOffset -> String -> String
getError offset s =
  replace "BYTES" (showHex offset "h") $ process $ viewL $ lines s
  where
    process Nothing = ""
    process (Just (e, rules)) = foldl apply e rules
    apply e ('R' : 'E' : 'P' : 'L' : 'A' : 'C' : 'E' : '/' : r) =
      case splitOn "/" r of
        (a : b : []) -> if e == a then b else e
        _ -> e
    apply _ ('E' : 'R' : 'R' : 'O' : 'R' : '/' : r) = r
    apply e _  = e

expect :: (Show a, Eq a) => a -> GetEx a -> GetEx ()
expect expected getter = do
  offset <- bytesReadEx
  actual <- getter
  if actual /= expected
    then fail $ showHex offset $ showString "h: " $ shows expected $ showString " expected, but " $ shows actual " provided."
    else return ()

sign :: GetEx T3Sign
sign = t3SignNew <$> ex getWord32le

size :: GetEx Word32
size = ex getWord32le

gap :: GetEx ()
gap = expect 0 $ ex getWord64le

data T3Field = T3BinaryField T3Sign ByteString deriving (Eq, Show)
data T3Record = T3Record T3Sign [T3Field] deriving (Eq, Show)
data T3File = T3File [T3Field] [T3Record] deriving (Eq, Show)

binaryField :: GetEx ByteString
binaryField = ex getRemainingLazyByteString

fieldBody :: T3Sign -> Word32 -> GetEx T3Field
fieldBody s _ = (T3BinaryField s) <$> binaryField

field :: GetEx T3Field
field = do
  s <- sign
  z <- size
  labelEOF (showString "BYTES: unexpected end of field (" $ shows s ")") $ isolateEx (fromIntegral z) $ fieldBody s z

recordBody :: GetEx [T3Field]
recordBody = whileM (not <$> isEmptyEx) field

recordTail :: T3Sign -> GetEx [T3Field]
recordTail s = do
  z <- size
  gap
  labelEOF (showString "BYTES: unexpected end of record (" $ shows s $ showString " " $ shows z ")") $ isolateEx (fromIntegral z) recordBody

record :: GetEx T3Record
record = do
  s <- sign
  t <- recordTail s
  return $ T3Record s t
  
fileSignature :: GetEx ()
fileSignature = labelError "File format not recognized." $ expect (T3Mark TES3) sign

file :: GetEx T3File
file = labelEOF "BYTES: unexpected end of file." $ do
  fileSignature
  header <- recordTail $ T3Mark TES3
  records <- whileM (not <$> isEmptyEx) record
  return $ T3File header records

getT3File :: Get T3File
getT3File = runEx 0 file
  
formatT3FileError :: ByteOffset -> String -> String
formatT3FileError = getError

runGetT3File :: ByteString -> Either String T3File
runGetT3File b =
  case pushEndOfInput $ runGetIncremental getT3File `pushChunks` b of
    Fail _ offset e -> Left $ formatT3FileError offset e
    Done (SB.null -> False) offset _ -> Left $ showHex offset "h: end of file expected."
    Done _ _ f -> Right f
    _ -> Left "Internal error."

disassembly :: ByteString -> Either String String
disassembly b = do
  _ <- runGetT3File b
  return "ESP\n"
