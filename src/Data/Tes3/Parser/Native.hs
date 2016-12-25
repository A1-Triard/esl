module Data.Tes3.Parser.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Utils

pT3FileSignature :: T.Parser ()
pT3FileSignature = do
  void $ Tp.string "3SET"
  Tp.endOfLine

t3FileRef :: T.Parser T3FileRef
t3FileRef = do
  n <- pNulledRun
  void $ Tp.char ' '
  z <- Tp.decimal
  Tp.endOfLine
  return $ T3FileRef n z

pT3FileHeader :: T.Parser T3FileHeader
pT3FileHeader = do
  void $ Tp.string "VERSION "
  version <- Tp.decimal
  Tp.endOfLine
  void $ Tp.string "TYPE "
  file_type <- pT3FileType
  Tp.endOfLine
  void $ Tp.string "AUTHOR "
  author <- pLine
  void $ Tp.string "DESCRIPTION"
  Tp.endOfLine
  description <- pLines
  refs <- many t3FileRef
  return $ T3FileHeader version file_type author description refs

pT3Record :: T.Parser T3Record
pT3Record = do
  s <- pT3Sign
  g <- Tp.option 0 $ Tp.char ' ' >> Tp.decimal
  Tp.endOfLine
  fields <- many $ t3Field s
  return $ T3Record s g fields

t3Field :: T3Sign -> T.Parser T3Field
t3Field record_sign = do
  s <- pT3Sign
  t3FieldBody (t3FieldType record_sign s) s

pFloat :: T.Parser Float
pFloat = (double2Float <$> Tp.double) <|> (const (0/0) <$> Tp.string "NaN")

t3FieldBody :: T3FieldType -> T3Sign -> T.Parser T3Field
t3FieldBody T3Binary s = do
  void $ Tp.char ' '
  b <- decode <$> C.pack <$> ST.unpack <$> Tp.takeTill Tp.isEndOfLine
  Tp.endOfLine
  case b of
    Left e -> fail e
    Right r -> return $ T3BinaryField s r
t3FieldBody T3String s = do
  void $ Tp.char ' '
  t <- pNulledLine
  return $ T3StringField s t
t3FieldBody T3Multiline s = do
  Tp.endOfLine
  t <- pLines
  return $ T3MultilineField s t
t3FieldBody T3AdjustableMultiline s = do
  Tp.endOfLine
  t <- pLines
  return $ T3MultilineField s t
t3FieldBody T3MultiString s = do
  void $ Tp.char ' '
  t <- pNames
  return $ T3MultiStringField s t
t3FieldBody T3Ref s = do
  void $ Tp.char ' '
  n <- Tp.decimal
  void $ Tp.char ' '
  t <- pLine
  return $ T3RefField s n t
t3FieldBody (T3FixedString _) s = do
  void $ Tp.char ' '
  t <- pNulledLine
  return $ T3FixedStringField s t
t3FieldBody T3Float s = do
  void $ Tp.char ' '
  v <- pFloat
  Tp.endOfLine
  return $ T3FloatField s v
t3FieldBody T3Int s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3IntField s v
t3FieldBody T3Short s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3ShortField s v
t3FieldBody T3Long s = do
  void $ Tp.char ' '
  v <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3LongField s v
t3FieldBody T3Byte s = do
  void $ Tp.char ' '
  v <- Tp.decimal
  Tp.endOfLine
  return $ T3ByteField s v
t3FieldBody T3Compressed s = do
  void $ Tp.char ' '
  b <- decode <$> C.pack <$> ST.unpack <$> Tp.takeTill Tp.isEndOfLine
  Tp.endOfLine
  case b of
    Left e -> fail e
    Right r -> return $ T3CompressedField s r
t3FieldBody T3Ingredient s = do
  Tp.endOfLine
  void $ Tp.string "    "
  weight <- pFloat
  void $ Tp.char ' '
  value <- Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  e1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  e4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  s1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  s4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  void $ Tp.string "    "
  a1 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a2 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a3 <- Tp.signed Tp.decimal
  void $ Tp.char ' '
  a4 <- Tp.signed Tp.decimal
  Tp.endOfLine
  return $ T3IngredientField s $ T3IngredientData
    weight value
    (T3IngredientEffects e1 e2 e3 e4)
    (T3IngredientSkills s1 s2 s3 s4)
    (T3IngredientAttributes a1 a2 a3 a4)
t3FieldBody T3Script s = do
  void $ Tp.char ' '
  name <- pRun
  void $ Tp.char ' '
  shorts <- Tp.decimal
  void $ Tp.char ' '
  longs <- Tp.decimal
  void $ Tp.char ' '
  floats <- Tp.decimal
  void $ Tp.char ' '
  data_size <- Tp.decimal
  void $ Tp.char ' '
  var_table_size <- Tp.decimal
  Tp.endOfLine
  return $ T3ScriptField s $ T3ScriptHeader
    name
    shorts longs floats
    data_size var_table_size
