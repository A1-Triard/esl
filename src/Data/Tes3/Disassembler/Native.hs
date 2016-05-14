module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3

newtype GetEx e a = GetEx ((ByteOffset -> e, ByteOffset) -> Get (Either e a))
instance Monad (GetEx e) where
  GetEx x >>= y =
    GetEx result
    where
      result u = do
        p <- x u
        case p of
          Left t -> return $ Left t
          Right t ->
            let GetEx z = y t in
            z u
  fail = noFail . fail

instance Functor (GetEx e) where
  fmap f (GetEx x) = GetEx $ \u -> (f <$>) <$> x u

instance Applicative (GetEx e) where
  pure a = GetEx $ const $ return $ Right a
  GetEx f <*> GetEx x =
    GetEx result
    where
      result u = do
        xx <- x u
        case xx of
          Left t -> return $ Left t
          Right xxx -> do
            ff <- f u
            case ff of
              Left t -> return $ Left t
              Right fff -> return $ Right $ fff xxx

instance Semigroup e => Alternative (GetEx e) where
  empty = noFail empty
  GetEx a <|> GetEx b =
    GetEx result
    where
      result u =
        try_a <|> try_b
        where
          try_b = b u
          try_a = do
            aa <- a u
            case aa of
              Right t -> return $ Right t
              Left ea -> withLeft (ea <>) <$> try_b

instance Semigroup e => MonadPlus (GetEx e) where

bytesReadEx :: GetEx e ByteOffset
bytesReadEx = GetEx $ \(_, base_offset) -> (Right . (base_offset +)) <$> bytesRead

failEx :: e -> GetEx e a
failEx t = GetEx $ const $ return $ Left t

onFail :: Get a -> e -> GetEx e a
onFail getter e = GetEx $ const $ (Right <$> getter) <|> (return $ Left e)

noFail :: Get a -> GetEx e a
noFail getter = GetEx $ const $ Right <$> getter

eofFail :: Get a -> GetEx e a
eofFail getter =
  GetEx $ \(eof_error, base_offset) -> (Right <$> getter) <|> get_eof eof_error base_offset
  where
    get_eof eof_error base_offset = do
      whileM_ (not <$> isEmpty) $ skip 1
      offset <- bytesRead
      return $ Left $ eof_error $ base_offset + offset

withLeft :: (a -> a') -> Either a b -> Either a' b
withLeft f (Left x) = Left $ f x
withLeft _ (Right x) = Right x

eofError :: GetEx e (ByteOffset -> e)
eofError = GetEx $ \(eof_error, _) -> return $ Right eof_error

withFail :: (e -> e') -> GetEx e a -> (ByteOffset -> e) -> GetEx e' a
withFail f (GetEx getter) eof_error = GetEx $ \(_, base_offset) -> withLeft f <$> getter (eof_error, base_offset)

withFail_ :: GetEx e a -> (e -> e) -> GetEx e a
withFail_ (GetEx getter) f = GetEx $ \u -> withLeft f <$> getter u

withGet :: (Get (Either e a) -> Get (Either e a')) -> GetEx e a -> GetEx e a'
withGet f (GetEx getter) = GetEx $ \u -> f $ getter u

withEofError :: GetEx e a -> (ByteOffset -> e) -> GetEx e a
withEofError (GetEx getter) eof_error = GetEx $ \(_, base_offset) -> getter (eof_error, base_offset)

labelEx :: String -> GetEx e a -> GetEx e a
labelEx s = withGet $ label s

isolateEx :: Int -> GetEx e a -> (ByteOffset -> e) -> (ByteOffset -> e) -> GetEx e a
isolateEx n (GetEx getter) tail_error inner_eof_error =
  GetEx result
  where
    result (eof_error, base_offset) = do
      offset <- bytesRead
      (isolate n $ g (inner_eof_error, base_offset + offset)) <|> get_again eof_error base_offset
    g (eof_error, base_offset) = do
      x <- getter (eof_error, base_offset)
      case x of
        Right t -> return $ Right t
        Left t -> do
          whileM_ (not <$> isEmpty) $ skip 1
          return $ Left t
    get_again eof_error base_offset = do
      _ <- getter (eof_error, base_offset)
      offset <- bytesRead
      return $ Left $ tail_error $ base_offset + offset      

isEmptyEx :: GetEx e Bool
isEmptyEx = noFail isEmpty

runEx :: ByteOffset -> GetEx e a -> (ByteOffset -> e) -> Get (Either e a)
runEx base_offset (GetEx getter) eof_error = getter (eof_error, base_offset)

getWord32leEx :: GetEx e Word32
getWord32leEx = eofFail getWord32le

getWord64leEx :: GetEx e Word64
getWord64leEx = eofFail getWord64le

getRemainingLazyByteStringEx :: GetEx e ByteString
getRemainingLazyByteStringEx = noFail getRemainingLazyByteString

expect :: (Show a, Eq a) => a -> GetEx String a -> GetEx String ()
expect expected getter = do
  offset <- bytesReadEx
  actual <- getter
  if actual /= expected
    then failEx $ showHex offset $ showString "h: " $ shows expected $ showString " expected, but " $ shows actual " provided."
    else return ()

sign :: GetEx String T3Sign
sign = t3SignNew <$> getWord32leEx

size :: GetEx String Word32
size = getWord32leEx

gap :: GetEx String ()
gap = expect 0 getWord64leEx

data T3Field = T3BinaryField T3Sign ByteString deriving (Eq, Show)
data T3Record = T3Record T3Sign [T3Field] deriving (Eq, Show)
data T3File = T3File [T3Field] [T3Record] deriving (Eq, Show)

binaryField :: GetEx String ByteString
binaryField = getRemainingLazyByteStringEx

fieldBody :: T3Sign -> Word32 -> GetEx String T3Field
fieldBody s _ = (T3BinaryField s) <$> binaryField

field :: GetEx String T3Field
field = do
  s <- sign
  z <- size
  isolateEx (fromIntegral z) (fieldBody s z)
    (\b -> showHex b $ "h: end of field expected")
    (\b -> showHex b $ showString "h: unexpected end of field (" $ shows s ")")

recordBody :: GetEx String [T3Field]
recordBody = whileM (not <$> isEmptyEx) field

recordTail :: T3Sign -> GetEx String [T3Field]
recordTail s = do
  z <- size
  gap
  isolateEx (fromIntegral z) recordBody
    (\b -> showHex b $ "h: end of record expected")
    (\b -> showHex b $ showString "h: unexpected end of record (" $ shows s $ showString " " $ shows z ")")

record :: GetEx String T3Record
record = do
  s <- sign
  t <- recordTail s
  return $ T3Record s t
  
fileSignature :: GetEx String ()
fileSignature = expect (T3Mark TES3) sign `withFail_` const "File format not recognized."

file :: GetEx String T3File
file = do
  fileSignature
  header <- recordTail $ T3Mark TES3
  records <- whileM (not <$> isEmptyEx) record
  return $ T3File header records

getT3File :: Get (Either String T3File)
getT3File = runEx 0 file $ \b -> showHex b "h: unexpected end of file."

runGetT3File :: ByteString -> Either String T3File
runGetT3File b =
  case pushEndOfInput $ runGetIncremental getT3File `pushChunks` b of
    Fail _ offset e -> Left $ showHex offset $ showString "h: " e
    Done _ _ (Left e) -> Left e
    Done (SB.null -> False) offset (Right _) -> Left $ showHex offset "h: end of file expected."
    Done _ _ (Right f) -> Right f
    _ -> Left "Internal error."

disassembly :: ByteString -> Either String String
disassembly b = do
  _ <- runGetT3File b
  return "ESP\n"
