module Data.Tes3.Get.Native where

#include <haskell>
import Data.Tes3

se :: String -> ByteOffset -> String
se s off = showHex off $ "h: " ++ s

type StringE = ByteOffset -> String

ee :: Either e (Either e a) -> Either e a
ee (Left e) = Left e
ee (Right (Left e)) = Left e
ee (Right (Right a)) = Right a

expect :: (Show a, Eq a) => a -> Get e a -> Get (Either StringE e) ()
expect expected getter = do
  offset <- totalBytesRead
  actual <- onError Right getter
  if actual /= expected
    then failG $ Left $ const $ concat [showHex offset $ "h: ", shows expected " expected, but ", shows actual " provided."]
    else return ()

sign :: Get () T3Sign
sign = t3SignNew <$> getWord32le

size :: Get () Word32
size = getWord32le

flags :: Get (Either StringE ()) T3Flags
flags = do
  offset <- totalBytesRead
  w <- onError Right getWord64le
  case t3FlagsNew w of
    Nothing -> failG $ Left $ const $ showHex offset $ "h: invalid record flags (" ++ showHex w "h)."
    Just f -> return f

binaryField :: Get e ByteString
binaryField = getRemainingLazyByteString

stringField :: Get e Text
stringField = t3StringNew <$> getRemainingLazyByteString

multilineField :: (Text -> Text) -> Get e [Text]
multilineField adjust = T.splitOn "\r\n" <$> adjust <$> t3StringNew <$> getRemainingLazyByteString

multiStringField :: Get e [Text]
multiStringField = T.splitOn "\0" <$> t3StringNew <$> getRemainingLazyByteString

dialField :: Bool -> Get StringE (Either Word32 T3DialType)
dialField del =
  if del
    then Left <$> getWord32le `withError` se "unexpected end of field."
    else do
      b <- getWord8 `withError` se "unexpected end of field."
      case t3DialTypeNew b of
        Just t -> return $ Right t
        Nothing -> failG $ se "invalid dial type."

refField :: Get () (Int32, Text)
refField = do
  z <- getInt32le
  n <- getLazyByteString 32
  return (z, T.dropWhileEnd (== '\0') $ t3StringNew n)

fixedStringField :: Word32 -> Get () Text
fixedStringField z = T.dropWhileEnd (== '\0') <$> t3StringNew <$> getLazyByteString (fromIntegral z)

floatField :: Get () Float
floatField = wordToFloat <$> getWord32le

compressedField :: Get e ByteString
compressedField = GZip.compress <$> getRemainingLazyByteString

ingredientField :: Get () T3IngredientData
ingredientField = do
  weight <- wordToFloat <$> getWord32le
  value <- getWord32le
  e1 <- getInt32le
  e2 <- getInt32le
  e3 <- getInt32le
  e4 <- getInt32le
  s1 <- getInt32le
  s2 <- getInt32le
  s3 <- getInt32le
  s4 <- getInt32le
  a1 <- getInt32le
  a2 <- getInt32le
  a3 <- getInt32le
  a4 <- getInt32le
  return $ T3IngredientData weight value
    (T3IngredientEffects e1 e2 e3 e4)
    (T3IngredientSkills s1 s2 s3 s4)
    (T3IngredientAttributes a1 a2 a3 a4)

scriptField :: Get () T3ScriptHeader
scriptField = do
  name <- T.dropWhileEnd (== '\0') <$> t3StringNew <$> getLazyByteString 32
  shorts <- getWord32le
  longs <- getWord32le
  floats <- getWord32le
  data_size <- getWord32le
  var_table_size <- getWord32le
  return $ T3ScriptHeader name shorts longs floats data_size var_table_size

fileHeaderData :: Get (Either StringE ()) T3FileHeader
fileHeaderData = do
  version <- getWord32le `withError` Right ()
  file_type_value <- getWord32le `withError` Right ()
  file_type <- case t3FileTypeNew file_type_value of
    Nothing -> failG $ Left $ const $ "Unknown file type: " ++ show file_type_value ++ "."
    Just x -> return x
  author <- T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 32 `withError` Right ()
  description <- T.splitOn "\r\n" <$> T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 256 `withError` Right ()
  void $ getWord32le `withError` Right ()
  return $ T3FileHeader version file_type author description

fieldBody :: Bool -> T3Sign -> T3Sign -> Word32 -> Get (Either StringE ()) T3Field
fieldBody adjust record_sign s field_size =
  f (t3FieldType record_sign s)
  where
    f (T3FixedString z) = onError Right $ T3StringField s . (`T.snoc` '\0') <$> fixedStringField z
    f (T3String a) = onError Right $ T3StringField s <$> (if adjust then a else id) <$> stringField
    f (T3Multiline a) = onError Right $ T3MultilineField s <$> multilineField (if adjust then a else id)
    f T3MultiString = onError Right $ T3MultiStringField s <$> multiStringField
    f T3Ref = onError Right $ (\(z, n) -> T3RefField s z n) <$> refField
    f T3Binary = onError Right $ T3BinaryField s <$> binaryField
    f T3Float = onError Right $ T3FloatField s <$> floatField
    f T3Int = onError Right $ T3IntField s <$> getInt32le
    f T3Short = onError Right $ T3ShortField s <$> getInt16le
    f T3Long = onError Right $ T3LongField s <$> getInt64le
    f T3Byte = onError Right $ T3ByteField s <$> getWord8
    f T3Compressed = onError Right $ T3CompressedField s <$> compressedField
    f T3Ingredient = onError Right $ T3IngredientField s <$> ingredientField
    f T3Script = onError Right $ T3ScriptField s <$> scriptField
    f T3Dial = onError Left $ T3DialField s <$> dialField (field_size /= 1)
    f T3None = (const $ T3NoneField s) <$> expect 0 getWord32le
    f T3Header = T3HeaderField s <$> fileHeaderData

field :: Bool -> T3Sign -> Get StringE T3Field
field adjust record_sign = do
  s <- sign `withError` se "unexpected end of field."
  z <- size `withError` se "unexpected end of field."
  let body = onError (either id $ const $ se "unexpected end of field.") $ fieldBody adjust record_sign s z
  isolate (fromIntegral z) body $ \c -> se $ "field size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."

recordBody :: Bool -> T3Sign -> Get StringE [T3Field]
recordBody adjust s = whileM (not <$> isEmpty) $ field adjust s

recordTail :: Bool -> T3Sign -> Get (Either StringE ()) (T3Flags, [T3Field])
recordTail adjust s = do
  z <- size `withError` Right ()
  g <- flags
  f <- onError Left $ isolate (fromIntegral z) (recordBody adjust s) $ \c -> se $ "record size mismatch: " ++ show z ++ " expected, but " ++ show c ++ " consumed."
  return (g, f)

getT3Record :: Bool -> Get StringE T3Record
getT3Record adjust = do
  s <- sign `withError` se "unexpected end of record."
  (g, f) <- onError (either id (const $ se "unexpected end of record.")) $ recordTail adjust s
  return $ T3Record s g f
