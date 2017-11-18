--
-- Copyright 2016, 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Data.Tes3.Get
  ( getT3Record
  ) where

#include <haskell>
import Data.Tes3

se :: String -> ByteOffset -> String
se s off = showHex off $ "h: " ++ s

type StringE = ByteOffset -> String

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

multilineField :: Bool -> (Text -> Text) -> Get e [Text]
multilineField use_unix_newlines adjust = T.splitOn (if use_unix_newlines then "\n" else "\r\n") <$> adjust <$> t3StringNew <$> getRemainingLazyByteString

multiStringField :: Get e [Text]
multiStringField = T.splitOn "\0" <$> t3StringNew <$> getRemainingLazyByteString

dialField :: Word32 -> Get StringE (Either Word32 T3DialType)
dialField field_size =
  if field_size == 4
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

floatField :: Get () (Either Word32 Float)
floatField = do
  w <- getWord32le
  let f = wordToFloat w
  return $ if isNaN f && w /= 0xFFFFFFFF
    then Left w
    else Right f

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

essNpcData :: Get () T3EssNpcData
essNpcData = do
  disposition <- getInt16le
  reputation <- getInt16le
  index <- getWord32le
  return $ T3EssNpcData disposition reputation index

npcDataChar :: Get () T3NpcDataChar
npcDataChar = do
  strength <- getWord8
  intelligence <- getWord8
  willpower <- getWord8
  agility <- getWord8
  speed <- getWord8
  endurance <- getWord8
  personality <- getWord8
  luck <- getWord8
  block <- getWord8
  armorer <- getWord8
  mediumArmor <- getWord8
  heavyArmor <- getWord8
  bluntWeapon <- getWord8
  longBlade <- getWord8
  axe <- getWord8
  spear <- getWord8
  athletics <- getWord8
  enchant <- getWord8
  destruction <- getWord8
  alteration <- getWord8
  illusion <- getWord8
  conjuration <- getWord8
  mysticism <- getWord8
  restoration <- getWord8
  alchemy <- getWord8
  unarmored <- getWord8
  security <- getWord8
  sneak <- getWord8
  acrobatics <- getWord8
  lightArmor <- getWord8
  shortBlade <- getWord8
  marksman <- getWord8
  mercantile <- getWord8
  speechcraft <- getWord8
  handToHand <- getWord8
  faction <- getWord8
  health <- getInt16le
  magicka <- getInt16le
  fatigue <- getInt16le
  return $ T3NpcDataChar
    strength intelligence willpower agility speed endurance personality luck block armorer mediumArmor heavyArmor
    bluntWeapon longBlade axe spear athletics enchant destruction alteration illusion conjuration mysticism restoration
    alchemy unarmored security sneak acrobatics lightArmor shortBlade marksman mercantile speechcraft handToHand faction
    health magicka fatigue

npcData :: Word32 -> Get (Either StringE ()) T3NpcData
npcData field_size = do
  let with_char = field_size == 52
  level <- getWord16le `withError` Right ()
  (ch, disposition, reputation, rank) <- if with_char
    then do
      c <- npcDataChar `withError` Right ()
      disp <- getInt8 `withError` Right ()
      rep <- getInt8 `withError` Right ()
      rk <- getInt8 `withError` Right ()
      expect 0 getWord8
      return (Right c, disp, rep, rk)
    else do
      disp <- getInt8 `withError` Right ()
      rep <- getInt8 `withError` Right ()
      rk <- getInt8 `withError` Right ()
      u1 <- getWord8 `withError` Right ()
      u2 <- getWord16le `withError` Right ()
      return ( Left $ (fromIntegral u1 `shiftL` 16) .|. fromIntegral u2, disp, rep, rk)
  gold <- getInt32le `withError` Right ()
  return $ T3NpcData level disposition reputation rank gold ch

fieldBody :: Bool -> T3Sign -> T3Sign -> Word32 -> Get (Either StringE ()) T3Field
fieldBody adjust record_sign s field_size =
  f (t3FieldType record_sign s)
  where
    f (T3FixedString z) = onError Right $ T3StringField s . (`T.snoc` '\0') <$> fixedStringField z
    f (T3String a) = onError Right $ T3StringField s <$> (if adjust then a else id) <$> stringField
    f (T3Multiline use_unix_newlines a) = onError Right $ T3MultilineField s <$> multilineField use_unix_newlines (if adjust then a else id)
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
    f T3Dial = onError Left $ T3DialField s <$> dialField field_size
    f T3None = (const $ T3NoneField s) <$> expect 0 getWord32le
    f T3Header = T3HeaderField s <$> fileHeaderData
    f T3EssNpc = onError Right $ T3EssNpcField s <$> essNpcData
    f T3Npc = T3NpcField s <$> npcData field_size

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
