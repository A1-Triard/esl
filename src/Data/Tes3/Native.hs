module Data.Tes3.Native where

#include <haskell>

data T3Mark
  = TES3 | HEDR | MAST | DATA | GLOB | NAME | FNAM | FORM
  | GMST | GMDT | ACTI | ALCH | APPA | ARMO | BODY | BOOK
  | BSGN | CELL | CLAS | CLOT | CNTC | CONT | CREA | CREC
  | DIAL | DOOR | ENCH | FACT | INFO | INGR | LAND | LEVC
  | LEVI | LIGH | LOCK | LTEX | MGEF | MISC | NPC_ | NPCC
  | PGRD | PROB | RACE | REGN | REPA | SCPT | SKIL | SNDG
  | SOUN | SPEL | SSCR | STAT | WEAP | SAVE | JOUR | QUES
  | GSCR | PLAY | CSTA | GMAP | DIAS | WTHR | KEYS | DYNA
  | ASPL | ACTC | MPRJ | PROJ | DCOU | MARK | FILT | DBGP
  | STRV | INTV | FLTV | SCHD | SCVR | SCDT | SCTX | MODL
  | IRDT | SCRI | ITEX | CNDT | FLAG | MCDT | NPCO | AADT
  | CTDT | RNAM | INDX | CNAM | ANAM | BNAM | KNAM | NPDT
  | AIDT | DODT | AI_W | CLDT | BYDT | RGNN | AODT | NAM5
  | DESC | WHGT | FADT | AMBI | FRMR | RADT | NAM0 | NPCS
  | DNAM | XSCL | SKDT | DELE | MEDT | PTEX | CVFX | BVFX
  | HVFX | AVFX | BSND | CSND | HSND | ASND | WEAT | SNAM
  | INAM | NNAM | PNAM | ONAM
  deriving (Eq, Ord, Enum, Bounded, Show)

data T3Sign = T3Mark T3Mark | T3Sign Word32 deriving Eq

instance Show T3Sign where
  show (T3Mark m) = show m
  show (T3Sign s) =
    let (a, b, c, d) = toBytes s in
    [chr $ fromIntegral a, chr $ fromIntegral b, chr $ fromIntegral c, chr $ fromIntegral d]

pT3Sign :: T.Parser T3Sign
pT3Sign = do
  a <- Tp.anyChar
  b <- Tp.anyChar
  c <- Tp.anyChar
  d <- Tp.anyChar
  return $ t3SignNew $ fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)

fromBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fromBytes a b c d
  =  (fromIntegral a)
  .|. (shift (fromIntegral b) 8)
  .|. (shift (fromIntegral c) 16)
  .|. (shift (fromIntegral d) 24)
  
toBytes :: Word32 -> (Word8, Word8, Word8, Word8)
toBytes w =
  ( fromIntegral $ w .&. 0xFF
  , fromIntegral $ (.&. 0xFF) $ shift w (-8)
  , fromIntegral $ (.&. 0xFF) $ shift w (-16)
  , fromIntegral $ (.&. 0xFF) $ shift w (-24)
  )

t3MarkValueSlow :: T3Mark -> Word32
t3MarkValueSlow mark =
  calc (show mark)
  where
    calc [a, b, c, d] = fromBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)
    calc _ = 0x0BAD0BAD

t3MarkValues :: S.Map T3Mark Word32
t3MarkValues = SM.fromList [(m, t3MarkValueSlow m) | m <- [minBound .. maxBound]]

t3MarkValue :: T3Mark -> Word32
t3MarkValue m = fromMaybe 0x0BAD0BAD $ SM.lookup m t3MarkValues

t3SignValue :: T3Sign -> Word32
t3SignValue (T3Mark m) = t3MarkValue m
t3SignValue (T3Sign s) = s

t3MarkNews :: S.Map Word32 T3Mark
t3MarkNews = SM.fromList [(t3MarkValue m, m) | m <- [minBound .. maxBound]]

t3MarkNew :: Word32 -> Maybe T3Mark
t3MarkNew w = SM.lookup w t3MarkNews

t3SignNew :: Word32 -> T3Sign
t3SignNew w = fromMaybe (T3Sign w) $ T3Mark <$> t3MarkNew w

data T3FileType = ESP | ESM | ESS deriving (Eq, Enum, Show, Bounded)

t3FileTypeValue :: T3FileType -> Word32
t3FileTypeValue ESP = 0
t3FileTypeValue ESM = 1
t3FileTypeValue ESS = 32

t3FileTypeNew :: Word32 -> Maybe T3FileType
t3FileTypeNew 0 = Just ESP
t3FileTypeNew 1 = Just ESM
t3FileTypeNew 32 = Just ESS
t3FileTypeNew _ = Nothing

pT3FileType :: T.Parser T3FileType
pT3FileType = foldl1 (<|>) [Tp.string (ST.pack $ show t) >> return t | t <- [minBound .. maxBound]]

data T3FieldType
  = T3Binary
  | T3String
  | T3Multiline
  | T3MultiString
  | T3Ref
  | T3FixedString Word32
  deriving (Eq, Show)

t3FieldType :: T3Sign -> T3Sign -> T3FieldType
t3FieldType _ (T3Mark ANAM) = T3String
t3FieldType _ (T3Mark ASND) = T3String
t3FieldType _ (T3Mark AVFX) = T3String
t3FieldType _ (T3Mark BNAM) = T3Multiline
t3FieldType _ (T3Mark BSND) = T3String
t3FieldType _ (T3Mark BVFX) = T3String
t3FieldType _ (T3Mark CNAM) = T3String
t3FieldType _ (T3Mark CSND) = T3String
t3FieldType _ (T3Mark CVFX) = T3String
t3FieldType _ (T3Mark DESC) = T3String
t3FieldType _ (T3Mark DNAM) = T3String
t3FieldType _ (T3Mark FNAM) = T3String
t3FieldType _ (T3Mark HSND) = T3String
t3FieldType _ (T3Mark HVFX) = T3String
t3FieldType _ (T3Mark INAM) = T3String
t3FieldType _ (T3Mark ITEX) = T3String
t3FieldType _ (T3Mark KNAM) = T3String
t3FieldType _ (T3Mark MODL) = T3String
t3FieldType _ (T3Mark NAME) = T3String
t3FieldType _ (T3Mark NNAM) = T3String
t3FieldType _ (T3Mark NPCO) = T3Ref
t3FieldType (T3Mark RACE) (T3Mark NPCS) = T3FixedString 32
t3FieldType _ (T3Mark NPCS) = T3String
t3FieldType _ (T3Mark ONAM) = T3String
t3FieldType _ (T3Mark PNAM) = T3String
t3FieldType _ (T3Mark PTEX) = T3String
t3FieldType (T3Mark FACT) (T3Mark RNAM) = T3FixedString 32
t3FieldType _ (T3Mark RNAM) = T3String
t3FieldType _ (T3Mark SCRI) = T3String
t3FieldType _ (T3Mark SCTX) = T3Multiline
t3FieldType (T3Mark SCPT) (T3Mark SCVR) = T3MultiString
t3FieldType _ (T3Mark SCVR) = T3String
t3FieldType _ (T3Mark SNAM) = T3String
t3FieldType _ (T3Mark STRV) = T3String
t3FieldType _ _ = T3Binary

data T3Field
  = T3BinaryField T3Sign ByteString
  | T3StringField T3Sign Text
  | T3FixedStringField T3Sign Text
  | T3MultilineField T3Sign [Text]
  | T3MultiStringField T3Sign [Text]
  | T3RefField T3Sign Word32 Text
  deriving (Eq, Show)
data T3Record = T3Record T3Sign Word64 [T3Field] deriving (Eq, Show)
data T3FileRef = T3FileRef Text Word64 deriving (Eq, Show)
data T3FileHeader = T3FileHeader Word32 T3FileType Text [Text] [T3FileRef] deriving (Eq, Show)

t3StringValue :: Text -> ByteString
t3StringValue = IC.convertFuzzy IC.Transliterate "UTF-8" "CP1251" . T.encodeUtf8

t3StringNew :: ByteString -> Text
t3StringNew = T.decodeUtf8 . IC.convert "CP1251" "UTF-8"
