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
  | INAM | NNAM | PNAM | ONAM | TNAM | ENAM | TEXT | VNML
  | VHGT | VCLR | VTEX | WNAM | NAM9 | KLST | PCDT | LNAM
  | FMAP | MAPD | MAPH | FGTN | LSHN | LSTN | ND3D | SLSD
  | ZNAM
  deriving (Eq, Ord, Enum, Bounded, Show)

data T3Sign = T3Mark T3Mark | T3Sign Word32 deriving Eq

instance Show T3Sign where
  show (T3Mark m) = show m
  show (T3Sign s) =
    let (a, b, c, d) = toBytes s in
    [chr $ fromIntegral a, chr $ fromIntegral b, chr $ fromIntegral c, chr $ fromIntegral d]

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

data T3FileType = ESP | ESM | ESS deriving (Eq, Ord, Enum, Bounded, Show)

t3FileTypeValue :: T3FileType -> Word32
t3FileTypeValue ESP = 0
t3FileTypeValue ESM = 1
t3FileTypeValue ESS = 32

t3FileTypeNew :: Word32 -> Maybe T3FileType
t3FileTypeNew 0 = Just ESP
t3FileTypeNew 1 = Just ESM
t3FileTypeNew 32 = Just ESS
t3FileTypeNew _ = Nothing

data T3DialType = T3Topic | T3Voice | T3Greeting | T3Persuasion | T3Journal deriving (Eq, Ord, Enum, Bounded, Show)

t3DialTypeValue :: T3DialType -> Word8
t3DialTypeValue T3Topic = 0
t3DialTypeValue T3Voice = 1
t3DialTypeValue T3Greeting = 2
t3DialTypeValue T3Persuasion = 3
t3DialTypeValue T3Journal = 4

t3DialTypeNew :: Word8 -> Maybe T3DialType
t3DialTypeNew 0 = Just T3Topic
t3DialTypeNew 1 = Just T3Voice
t3DialTypeNew 2 = Just T3Greeting
t3DialTypeNew 3 = Just T3Persuasion
t3DialTypeNew 4 = Just T3Journal
t3DialTypeNew _ = Nothing

data T3FieldType
  = T3Binary
  | T3String (Text -> Text)
  | T3Multiline Bool (Text -> Text)
  | T3MultiString
  | T3Ref
  | T3FixedString Word32
  | T3Float
  | T3Int
  | T3Short
  | T3Long
  | T3Byte
  | T3Compressed
  | T3Ingredient
  | T3Script
  | T3Dial
  | T3None
  | T3Header

t3FieldType :: T3Sign -> T3Sign -> T3FieldType
t3FieldType (T3Mark NPC_) (T3Mark ANAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType _ (T3Mark ANAM) = T3String id
t3FieldType _ (T3Mark ASND) = T3String id
t3FieldType _ (T3Mark AVFX) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark BNAM) = T3String $ T.takeWhile (/= '\0')
t3FieldType (T3Mark BODY) (T3Mark BNAM) = T3String $ T.takeWhile (/= '\0')
t3FieldType (T3Mark CELL) (T3Mark BNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark CLOT) (T3Mark BNAM) = T3String $ T.takeWhile (/= '\0')
t3FieldType (T3Mark CONT) (T3Mark BNAM) = T3Multiline False $ T.takeWhile (/= '\0')
t3FieldType (T3Mark INFO) (T3Mark BNAM) = T3Multiline False $ T.takeWhile (/= '\0')
t3FieldType (T3Mark NPC_) (T3Mark BNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark PCDT) (T3Mark BNAM) = T3String id
t3FieldType (T3Mark REGN) (T3Mark BNAM) = T3String id
t3FieldType _ (T3Mark BNAM) = T3Multiline False id
t3FieldType _ (T3Mark BSND) = T3String id
t3FieldType _ (T3Mark BVFX) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark CNAM) = T3String $ T.takeWhile (/= '\0')
t3FieldType (T3Mark KLST) (T3Mark CNAM) = T3Int
t3FieldType (T3Mark NPC_) (T3Mark CNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark REGN) (T3Mark CNAM) = T3Int
t3FieldType _ (T3Mark CNAM) = T3String id
t3FieldType _ (T3Mark CSND) = T3String id
t3FieldType _ (T3Mark CVFX) = T3String id
t3FieldType (T3Mark DIAL) (T3Mark DATA) = T3Dial
t3FieldType (T3Mark LAND) (T3Mark DATA) = T3Int
t3FieldType (T3Mark LEVC) (T3Mark DATA) = T3Int
t3FieldType (T3Mark LEVI) (T3Mark DATA) = T3Int
t3FieldType (T3Mark LTEX) (T3Mark DATA) = T3String id
t3FieldType (T3Mark SSCR) (T3Mark DATA) = T3String $ T.takeWhile (/= '\0')
t3FieldType (T3Mark TES3) (T3Mark DATA) = T3Long
t3FieldType (T3Mark DIAL) (T3Mark DELE) = T3None
t3FieldType _ (T3Mark DESC) = T3String id
t3FieldType _ (T3Mark DNAM) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark ENAM) = T3String id
t3FieldType (T3Mark PCDT) (T3Mark ENAM) = T3Long
t3FieldType (T3Mark CELL) (T3Mark FGTN) = T3String id
t3FieldType _ (T3Mark FLAG) = T3Int
t3FieldType _ (T3Mark FLTV) = T3Float
t3FieldType (T3Mark ACTI) (T3Mark FNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark PCDT) (T3Mark FNAM) = T3Binary
t3FieldType (T3Mark RACE) (T3Mark FNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType _ (T3Mark FNAM) = T3String id
t3FieldType (T3Mark CELL) (T3Mark FRMR) = T3Int
t3FieldType (T3Mark TES3) (T3Mark HEDR) = T3Header
t3FieldType _ (T3Mark HSND) = T3String id
t3FieldType _ (T3Mark HVFX) = T3String id
t3FieldType _ (T3Mark INAM) = T3String id
t3FieldType (T3Mark ARMO) (T3Mark INDX) = T3Byte
t3FieldType (T3Mark CLOT) (T3Mark INDX) = T3Byte
t3FieldType _ (T3Mark INDX) = T3Int
t3FieldType (T3Mark LAND) (T3Mark INTV) = T3Long
t3FieldType (T3Mark LEVC) (T3Mark INTV) = T3Short
t3FieldType (T3Mark LEVI) (T3Mark INTV) = T3Short
t3FieldType _ (T3Mark INTV) = T3Int
t3FieldType (T3Mark INGR) (T3Mark IRDT) = T3Ingredient
t3FieldType _ (T3Mark ITEX) = T3String id
t3FieldType (T3Mark NPC_) (T3Mark KNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark PCDT) (T3Mark KNAM) = T3Binary
t3FieldType _ (T3Mark KNAM) = T3String id
t3FieldType (T3Mark PCDT) (T3Mark LNAM) = T3Long
t3FieldType (T3Mark CELL) (T3Mark LSHN) = T3String id
t3FieldType (T3Mark CELL) (T3Mark LSTN) = T3String id
t3FieldType (T3Mark FMAP) (T3Mark MAPD) = T3Compressed
t3FieldType (T3Mark FMAP) (T3Mark MAPH) = T3Long
t3FieldType (T3Mark TES3) (T3Mark MAST) = T3String id
t3FieldType (T3Mark LIGH) (T3Mark MODL) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType _ (T3Mark MODL) = T3String id
t3FieldType (T3Mark CELL) (T3Mark NAM0) = T3Int
t3FieldType (T3Mark CELL) (T3Mark NAM5) = T3Int
t3FieldType (T3Mark CELL) (T3Mark NAM9) = T3Int
t3FieldType (T3Mark PCDT) (T3Mark NAM9) = T3Int
t3FieldType (T3Mark CELL) (T3Mark NAME) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark JOUR) (T3Mark NAME) = T3Multiline True id
t3FieldType (T3Mark SSCR) (T3Mark NAME) = T3String $ T.takeWhile (/= '\0')
t3FieldType _ (T3Mark NAME) = T3String id
t3FieldType (T3Mark CELL) (T3Mark ND3D) = T3Byte
t3FieldType (T3Mark INFO) (T3Mark NNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark LEVC) (T3Mark NNAM) = T3Byte
t3FieldType (T3Mark LEVI) (T3Mark NNAM) = T3Byte
t3FieldType _ (T3Mark NNAM) = T3String id
t3FieldType _ (T3Mark NPCO) = T3Ref
t3FieldType (T3Mark BSGN) (T3Mark NPCS) = T3FixedString 32
t3FieldType (T3Mark NPC_) (T3Mark NPCS) = T3FixedString 32
t3FieldType (T3Mark RACE) (T3Mark NPCS) = T3FixedString 32
t3FieldType _ (T3Mark NPCS) = T3String id
t3FieldType _ (T3Mark ONAM) = T3String id
t3FieldType (T3Mark INFO) (T3Mark PNAM) = T3String $ (`T.snoc` '\0') . T.takeWhile (/= '\0')
t3FieldType (T3Mark PCDT) (T3Mark PNAM) = T3Binary
t3FieldType _ (T3Mark PNAM) = T3String id
t3FieldType _ (T3Mark PTEX) = T3String id
t3FieldType _ (T3Mark RGNN) = T3String id
t3FieldType (T3Mark FACT) (T3Mark RNAM) = T3FixedString 32
t3FieldType _ (T3Mark RNAM) = T3String id
t3FieldType (T3Mark SCPT) (T3Mark SCHD) = T3Script
t3FieldType _ (T3Mark SCRI) = T3String id
t3FieldType _ (T3Mark SCTX) = T3Multiline False $ T.takeWhile (/= '\0')
t3FieldType (T3Mark SCPT) (T3Mark SCVR) = T3MultiString
t3FieldType _ (T3Mark SCVR) = T3String id
t3FieldType (T3Mark CELL) (T3Mark SLSD) = T3Short
t3FieldType (T3Mark PCDT) (T3Mark SNAM) = T3Binary
t3FieldType (T3Mark REGN) (T3Mark SNAM) = T3Binary
t3FieldType _ (T3Mark SNAM) = T3String id
t3FieldType _ (T3Mark STRV) = T3String id
t3FieldType (T3Mark ALCH) (T3Mark TEXT) = T3String id
t3FieldType (T3Mark BOOK) (T3Mark TEXT) = T3Multiline False $ T.takeWhile (/= '\0')
t3FieldType _ (T3Mark TEXT) = T3Multiline False id
t3FieldType _ (T3Mark TNAM) = T3String id
t3FieldType _ (T3Mark VCLR) = T3Compressed
t3FieldType _ (T3Mark VHGT) = T3Compressed
t3FieldType _ (T3Mark VNML) = T3Compressed
t3FieldType _ (T3Mark VTEX) = T3Compressed
t3FieldType _ (T3Mark WEAT) = T3Binary
t3FieldType (T3Mark CELL) (T3Mark WHGT) = T3Int
t3FieldType _ (T3Mark WNAM) = T3Compressed
t3FieldType (T3Mark CELL) (T3Mark XSCL) = T3Int
t3FieldType (T3Mark CELL) (T3Mark ZNAM) = T3Byte
t3FieldType _ _ = T3Binary

data T3IngredientEffects = T3IngredientEffects Int32 Int32 Int32 Int32 deriving (Eq, Show)
data T3IngredientSkills = T3IngredientSkills Int32 Int32 Int32 Int32 deriving (Eq, Show)
data T3IngredientAttributes = T3IngredientAttributes Int32 Int32 Int32 Int32 deriving (Eq, Show)

data T3IngredientData = T3IngredientData Float Word32 T3IngredientEffects T3IngredientSkills T3IngredientAttributes deriving (Eq, Show)
data T3ScriptHeader = T3ScriptHeader Text Word32 Word32 Word32 Word32 Word32 deriving (Eq, Show)

data T3FileHeader = T3FileHeader Word32 T3FileType Text [Text] deriving (Eq, Show)

data T3Field
  = T3BinaryField T3Sign ByteString
  | T3StringField T3Sign Text
  | T3MultilineField T3Sign [Text]
  | T3MultiStringField T3Sign [Text]
  | T3RefField T3Sign Int32 Text
  | T3FloatField T3Sign (Either Word32 Float)
  | T3IntField T3Sign Int32
  | T3ShortField T3Sign Int16
  | T3LongField T3Sign Int64
  | T3ByteField T3Sign Word8
  | T3CompressedField T3Sign ByteString
  | T3IngredientField T3Sign T3IngredientData
  | T3ScriptField T3Sign T3ScriptHeader
  | T3DialField T3Sign (Either Word32 T3DialType)
  | T3NoneField T3Sign
  | T3HeaderField T3Sign T3FileHeader
  deriving (Eq, Show)
data T3Flags = T3Flags
  { t3Persist :: Bool
  , t3Blocked :: Bool
  , t3Deleted :: Bool
  } deriving (Eq, Show)
data T3Record = T3Record T3Sign T3Flags [T3Field] deriving (Eq, Show)

fPersist, fBlocked, fDeleted :: Word64
fPersist = 0x40000000000
fBlocked = 0x200000000000
fDeleted = 0x2000000000

t3FlagsEmpty :: T3Flags
t3FlagsEmpty = T3Flags False False False

t3FlagsValue :: T3Flags -> Word64
t3FlagsValue f
   =  (if t3Persist f then fPersist else 0)
  .|. (if t3Blocked f then fBlocked else 0)
  .|. (if t3Deleted f then fDeleted else 0)

t3FlagsNew :: Word64 -> Maybe T3Flags
t3FlagsNew d =
  let cp = (d .&. fPersist) /= 0 in
  let d1 = (d .&. complement fPersist) in
  let bl = (d .&. fBlocked) /= 0 in
  let d2 = (d1 .&. complement fBlocked) in
  let del = (d .&. fDeleted) /= 0 in
  let d3 = (d2 .&. complement fDeleted) in
  if d3 == 0
    then Just $ T3Flags { t3Persist = cp, t3Blocked = bl, t3Deleted = del }
    else Nothing

t3StringValue :: Text -> ByteString
t3StringValue =
  B.pack . map convert . T.unpack
  where
    convert :: Char -> Word8
    convert c
      | c < '\128' = fromIntegral (ord c)
      | c >='А' && c <= 'я' = 192 + fromIntegral (ord c - 0x410)
      | c == '\x0402' = 128
      | c == '\x0403' = 129
      | c == '\x201A' = 130
      | c == '\x0453' = 131
      | c == '\x201E' = 132
      | c == '\x2026' = 133
      | c == '\x2020' = 134
      | c == '\x2021' = 135
      | c == '\x20AC' = 136
      | c == '\x2030' = 137
      | c == '\x0409' = 138
      | c == '\x2039' = 139
      | c == '\x040A' = 140
      | c == '\x040C' = 141
      | c == '\x040B' = 142
      | c == '\x040F' = 143
      | c == '\x0452' = 144
      | c == '\x2018' = 145
      | c == '\x2019' = 146
      | c == '\x201C' = 147
      | c == '\x201D' = 148
      | c == '\x2022' = 149
      | c == '\x2013' = 150
      | c == '\x2014' = 151
      | c == '\x2122' = 153
      | c == '\x0459' = 154
      | c == '\x203A' = 155
      | c == '\x045A' = 156
      | c == '\x045C' = 157
      | c == '\x045B' = 158
      | c == '\x045F' = 159
      | c == '\x00A0' = 160
      | c == '\x040E' = 161
      | c == '\x045E' = 162
      | c == '\x0408' = 163
      | c == '\x00A4' = 164
      | c == '\x0490' = 165
      | c == '\x00A6' = 166
      | c == '\x00A7' = 167
      | c == '\x0401' = 168
      | c == '\x00A9' = 169
      | c == '\x0404' = 170
      | c == '\x00AB' = 171
      | c == '\x00AC' = 172
      | c == '\x00AD' = 173
      | c == '\x00AE' = 174
      | c == '\x0407' = 175
      | c == '\x00B0' = 176
      | c == '\x00B1' = 177
      | c == '\x0406' = 178
      | c == '\x0456' = 179
      | c == '\x0491' = 180
      | c == '\x00B5' = 181
      | c == '\x00B6' = 182
      | c == '\x00B7' = 183
      | c == '\x0451' = 184
      | c == '\x2116' = 185
      | c == '\x0454' = 186 
      | c == '\x00BB' = 187
      | c == '\x0458' = 188
      | c == '\x0405' = 189
      | c == '\x0455' = 190
      | c == '\x0457' = 191
      | otherwise = 152

t3StringNew :: ByteString -> Text
t3StringNew =
  T.pack . map convert . B.unpack
  where
    convert :: Word8 -> Char
    convert c
      | c < 128 = chr (fromIntegral c)
      | c >= 192 = chr (0x410 + fromIntegral (c - 192))
      | c == 128 = '\x0402'
      | c == 129 = '\x0403'
      | c == 130 = '\x201A'
      | c == 131 = '\x0453'
      | c == 132 = '\x201E'
      | c == 133 = '\x2026'
      | c == 134 = '\x2020'
      | c == 135 = '\x2021'
      | c == 136 = '\x20AC'
      | c == 137 = '\x2030'
      | c == 138 = '\x0409'
      | c == 139 = '\x2039'
      | c == 140 = '\x040A'
      | c == 141 = '\x040C'
      | c == 142 = '\x040B'
      | c == 143 = '\x040F'
      | c == 144 = '\x0452'
      | c == 145 = '\x2018'
      | c == 146 = '\x2019'
      | c == 147 = '\x201C'
      | c == 148 = '\x201D'
      | c == 149 = '\x2022'
      | c == 150 = '\x2013'
      | c == 151 = '\x2014'
      | c == 152 = '\x2015'
      | c == 153 = '\x2122'
      | c == 154 = '\x0459'
      | c == 155 = '\x203A'
      | c == 156 = '\x045A'
      | c == 157 = '\x045C'
      | c == 158 = '\x045B'
      | c == 159 = '\x045F'
      | c == 160 = '\x00A0'
      | c == 161 = '\x040E'
      | c == 162 = '\x045E'
      | c == 163 = '\x0408'
      | c == 164 = '\x00A4'
      | c == 165 = '\x0490'
      | c == 166 = '\x00A6'
      | c == 167 = '\x00A7'
      | c == 168 = '\x0401'
      | c == 169 = '\x00A9'
      | c == 170 = '\x0404'
      | c == 171 = '\x00AB'
      | c == 172 = '\x00AC'
      | c == 173 = '\x00AD'
      | c == 174 = '\x00AE'
      | c == 175 = '\x0407'
      | c == 176 = '\x00B0'
      | c == 177 = '\x00B1'
      | c == 178 = '\x0406'
      | c == 179 = '\x0456'
      | c == 180 = '\x0491'
      | c == 181 = '\x00B5'
      | c == 182 = '\x00B6'
      | c == 183 = '\x00B7'
      | c == 184 = '\x0451'
      | c == 185 = '\x2116'
      | c == 186 = '\x0454'
      | c == 187 = '\x00BB'
      | c == 188 = '\x0458'
      | c == 189 = '\x0405'
      | c == 190 = '\x0455'
      | c == 191 = '\x0457'
      | otherwise = error "t3StringNew"
