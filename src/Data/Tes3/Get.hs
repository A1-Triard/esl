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
  ( T3Error (..)
  , getT3Record
  ) where

#include <haskell>
import Data.Tes3

data T3Error
  = forall a. Show a => T3Expected !Word64 !a !a
  | T3InvalidRecordFlags !Word64 !Word64
  | T3UnexpectedEndOfField !Word64
  | T3UnexpectedEndOfRecord !Word64
  | T3InvalidDialType !Word64
  | T3UnknownFileType !Word32
  | T3UnexpectedEndOfFile !Word64
  | T3FieldSizeMismatch !Word64 !Word32 !Word32
  | T3RecordSizeMismatch !Word64 !Word32 !Word32

instance Show T3Error where
  show (T3Expected offset expected actual) = concat [showHex offset $ "h: ", shows expected " expected, but ", shows actual " provided."]
  show (T3InvalidRecordFlags offset actual) = showHex offset $ "h: invalid record flags (" ++ showHex actual "h)."
  show (T3UnexpectedEndOfField offset) = showHex offset "h: unexpected end of field."
  show (T3UnexpectedEndOfRecord offset) = showHex offset "h: unexpected end of record."
  show (T3InvalidDialType offset) = showHex offset "h: invalid dial type."
  show (T3UnknownFileType file_type) = "Unknown file type: " ++ show file_type ++ "."
  show (T3UnexpectedEndOfFile offset) = showHex offset "h: unexpected end of file."
  show (T3FieldSizeMismatch offset expected actual) = concat [showHex offset "h: field size mismatch: ", shows expected " expected, but ", shows actual " consumed."]
  show (T3RecordSizeMismatch offset expected actual) = concat [showHex offset "h: record size mismatch: ", shows expected " expected, but ", shows actual " consumed."]

expect :: (DefaultDecodingState s, Monad m, Show a, Eq a) => a -> GetM s i o T3Error m a -> GetM s i o T3Error m ()
expect expected getter = do
  offset <- bytesRead
  actual <- getter
  if actual /= expected
    then throwError $ T3Expected offset expected actual
    else return ()

sign :: Get () T3Sign
sign = t3SignNew <$> getWord32le

size :: Get () Word32
size = getWord32le

flags :: Get T3Error T3Flags
flags = do
  offset <- bytesRead
  w <- getWord64le ?>> T3UnexpectedEndOfRecord <$> bytesRead
  case t3FlagsNew w of
    Nothing -> throwError $ T3InvalidRecordFlags offset w
    Just f -> return f

binaryField :: Get e ByteString
binaryField = getRemainingLazyByteString

stringField :: Get e Text
stringField = t3StringNew <$> getRemainingLazyByteString

multilineField :: Bool -> (Text -> Text) -> Get e [Text]
multilineField use_unix_newlines adjust = T.splitOn (if use_unix_newlines then "\n" else "\r\n") <$> adjust <$> t3StringNew <$> getRemainingLazyByteString

multiStringField :: Get e [Text]
multiStringField = T.splitOn "\0" <$> t3StringNew <$> getRemainingLazyByteString

dialField :: Word32 -> Get T3Error (Either Word32 T3DialType)
dialField field_size =
  if field_size == 4
    then Left <$> getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead
    else do
      offset <- bytesRead
      b <- getWord8 ?>> T3UnexpectedEndOfField <$> bytesRead
      case t3DialTypeNew b of
        Just t -> return $ Right t
        Nothing -> throwError $ T3InvalidDialType offset

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

fileHeaderData :: Get T3Error T3FileHeader
fileHeaderData = do
  version <- getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead
  file_type_value <- getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead
  file_type <- case t3FileTypeNew file_type_value of
    Nothing -> throwError $ T3UnknownFileType file_type_value
    Just x -> return x
  author <- T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 32 ?>> T3UnexpectedEndOfField <$> bytesRead
  description <- T.splitOn "\r\n" <$> T.dropWhileEnd (== '\0') <$> t3StringNew <$> B.fromStrict <$> getByteString 256 ?>> T3UnexpectedEndOfField <$> bytesRead
  void $ getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead
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

npcData :: Word32 -> Get T3Error T3NpcData
npcData field_size = do
  let with_char = field_size == 52
  level <- getWord16le ?>> T3UnexpectedEndOfField <$> bytesRead
  (ch, disposition, reputation, rank) <- if with_char
    then do
      c <- npcDataChar ?>> T3UnexpectedEndOfField <$> bytesRead
      disp <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      rep <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      rk <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      expect 0 $ getWord8 ?>> T3UnexpectedEndOfField <$> bytesRead
      return (Right c, disp, rep, rk)
    else do
      disp <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      rep <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      rk <- getInt8 ?>> T3UnexpectedEndOfField <$> bytesRead
      u1 <- getWord8 ?>> T3UnexpectedEndOfField <$> bytesRead
      u2 <- getWord16le ?>> T3UnexpectedEndOfField <$> bytesRead
      return ( Left $ (fromIntegral u1 `shiftL` 16) .|. fromIntegral u2, disp, rep, rk)
  gold <- getInt32le ?>> T3UnexpectedEndOfField <$> bytesRead
  return $ T3NpcData level disposition reputation rank gold ch

fieldBody :: Bool -> T3Sign -> T3Sign -> Word32 -> Get T3Error T3Field
fieldBody adjust record_sign s field_size =
  f (t3FieldType record_sign s)
  where
    f (T3FixedString z) = T3StringField s . (`T.snoc` '\0') <$> fixedStringField z ?>> T3UnexpectedEndOfField <$> bytesRead
    f (T3String a) = T3StringField s <$> (if adjust then a else id) <$> stringField ?>> T3UnexpectedEndOfField <$> bytesRead
    f (T3Multiline use_unix_newlines a) = T3MultilineField s <$> multilineField use_unix_newlines (if adjust then a else id) ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3MultiString = T3MultiStringField s <$> multiStringField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Ref = (\(z, n) -> T3RefField s z n) <$> refField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Binary = T3BinaryField s <$> binaryField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Float = T3FloatField s <$> floatField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Int = T3IntField s <$> getInt32le ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Short = T3ShortField s <$> getInt16le ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Long = T3LongField s <$> getInt64le ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Byte = T3ByteField s <$> getWord8 ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Compressed = T3CompressedField s <$> compressedField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Ingredient = T3IngredientField s <$> ingredientField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Script = T3ScriptField s <$> scriptField ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Dial = T3DialField s <$> dialField field_size
    f T3None = (const $ T3NoneField s) <$> expect 0 (getWord32le ?>> T3UnexpectedEndOfField <$> bytesRead)
    f T3Header = T3HeaderField s <$> fileHeaderData
    f T3EssNpc = T3EssNpcField s <$> essNpcData ?>> T3UnexpectedEndOfField <$> bytesRead
    f T3Npc = T3NpcField s <$> npcData field_size

field :: Bool -> T3Sign -> Get T3Error T3Field
field adjust record_sign = do
  s <- sign ?>> T3UnexpectedEndOfField <$> bytesRead
  z <- size ?>> T3UnexpectedEndOfField <$> bytesRead
  isolate (fromIntegral z) (fieldBody adjust record_sign s z)
    ?=>> either
      (maybe (T3UnexpectedEndOfRecord <$> bytesRead) (\ !c -> (\ !x -> T3FieldSizeMismatch x z (fromIntegral c)) <$> bytesRead))
      return

getT3Record :: Bool -> Get T3Error T3Record
getT3Record adjust = do
  s <- sign ?>> T3UnexpectedEndOfRecord <$> bytesRead
  z <- size ?>> T3UnexpectedEndOfRecord <$> bytesRead
  g <- flags
  f <- isolate (fromIntegral z) (whileM (not <$> N.nullE) $ field adjust s)
    ?=>> either
      (maybe (T3UnexpectedEndOfFile <$> bytesRead) (\ !c -> (\ !x -> T3RecordSizeMismatch x z (fromIntegral c)) <$> bytesRead))
      return
  return $ T3Record s g f
