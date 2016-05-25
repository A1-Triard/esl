module Data.Tes3.Utils.Native where

#include <haskell>

toHexDigit :: Integral a => a -> Char
toHexDigit 0 = '0'
toHexDigit 1 = '1'
toHexDigit 2 = '2'
toHexDigit 3 = '3'
toHexDigit 4 = '4'
toHexDigit 5 = '5'
toHexDigit 6 = '6'
toHexDigit 7 = '7'
toHexDigit 8 = '8'
toHexDigit 9 = '9'
toHexDigit 10 = 'A'
toHexDigit 11 = 'B'
toHexDigit 12 = 'C'
toHexDigit 13 = 'D'
toHexDigit 14 = 'E'
toHexDigit 15 = 'F'
toHexDigit _ = 'i'

pHexDigit :: Integral a => T.Parser a
pHexDigit =
  p_0 <|> p_1 <|> p_2 <|> p_3 <|> p_4 <|> p_5 <|> p_6 <|> p_7 <|> p_8 <|> p_9 <|> p_A <|> p_B <|> p_C <|> p_D <|> p_E <|> p_F
  where
    p_0 = Tp.char '0' >> return 0
    p_1 = Tp.char '1' >> return 1
    p_2 = Tp.char '2' >> return 2
    p_3 = Tp.char '3' >> return 3
    p_4 = Tp.char '4' >> return 4
    p_5 = Tp.char '5' >> return 5
    p_6 = Tp.char '6' >> return 6
    p_7 = Tp.char '7' >> return 7
    p_8 = Tp.char '8' >> return 8
    p_9 = Tp.char '9' >> return 9
    p_A = Tp.char 'A' >> return 10
    p_B = Tp.char 'B' >> return 11
    p_C = Tp.char 'C' >> return 12
    p_D = Tp.char 'D' >> return 13
    p_E = Tp.char 'E' >> return 14
    p_F = Tp.char 'F' >> return 15

toHexCode :: Char -> Text
toHexCode c =
  let o = ord c in
  let (x1, x2) = o `divMod` 16 in
  T.singleton (toHexDigit x1) <> T.singleton (toHexDigit x2)

pHexCode :: T.Parser Char
pHexCode = do
  x1 <- pHexDigit
  x2 <- pHexDigit
  return $ chr $ x1 * 16 + x2

writeEscapedChar :: Bool -> Bool -> Char -> Text
writeEscapedChar escape_percent escape_spaces c
  | c == '\0' = "\\0"
  | c == '\\' = "\\\\"
  | c == '\r' = "\\r"
  | c == '\n' = "\\n"
  | escape_percent && c == '%' = "\\%"
  | escape_spaces && c == ' ' = "\\ "
  | escape_spaces && c == '\t' = "\\t"
  | c == '\t' = "\t"
  | ord c < 32 || ord c == 255 = "\\x" <> toHexCode c
  | otherwise = T.singleton c

pEscapedChar :: Bool -> Bool -> T.Parser Char
pEscapedChar allow_percent allow_spaces = do
  c <- Tp.anyChar
  case c of
    '\r' -> fail "<r>"
    '\n' -> fail "<n>"
    '\\' -> tail
    '\t' -> if allow_spaces then return '\t' else fail "tab"
    ' ' -> if allow_spaces then return ' ' else fail "space"
    '%' -> if allow_percent then return '%' else fail "percent"
    s -> return s
  where
    tail = do
      c <- Tp.anyChar
      case c of
        '0' -> return '\0'
        '%' -> return '%'
        'r' -> return '\r'
        'n' -> return '\n'
        't' -> return '\t'
        '\\' -> return '\\'
        ' ' -> return ' '
        'x' -> pHexCode
        _ -> return '\xFFFD'

writeEscapedText :: Bool -> Bool -> Text -> Text
writeEscapedText escape_percent escape_spaces = T.concat . map (writeEscapedChar escape_percent escape_spaces) . T.unpack

pEscapedText :: Bool -> Bool -> T.Parser Text
pEscapedText allow_percent allow_spaces = T.pack <$> many (pEscapedChar allow_percent allow_spaces)

writeNulledText :: Bool -> Text -> Text
writeNulledText escape_spaces (T.stripSuffix "\0" -> Just s) = writeEscapedText True escape_spaces s
writeNulledText escape_spaces s = writeEscapedText True escape_spaces s <> "%"

writeNulledLine :: Text -> Text
writeNulledLine s = writeNulledText False s <> "\n"

writeNulledRun :: Text -> Text
writeNulledRun = writeNulledText True

pNulledText :: Bool -> T.Parser Text
pNulledText allow_spaces = do
  s <- pEscapedText False allow_spaces
  add_null <- Tp.option True (Tp.char '%' >> return False)
  if add_null
    then return $ s <> "\0"
    else return s

pNulledLine :: T.Parser Text
pNulledLine = do
  s <- pNulledText True
  Tp.endOfLine
  return s

pNulledRun :: T.Parser Text
pNulledRun = pNulledText False

writeText :: Bool -> Text -> Text
writeText = writeEscapedText False

writeLine :: Text -> Text
writeLine s = writeText False s <> "\n"

writeRun :: Text -> Text
writeRun = writeText True

pText :: Bool -> T.Parser Text
pText = pEscapedText True

pLine :: T.Parser Text
pLine = do
  s <- pText True
  Tp.endOfLine
  return s

pRun :: T.Parser Text
pRun = pText False

writeLines :: [Text] -> Text
writeLines = T.concat . map (\x -> "    " <> writeEscapedText False False x <> "\n")

pLines :: T.Parser [Text]
pLines = many $ do
  void $ Tp.string "    "
  s <- pEscapedText True True
  Tp.endOfLine
  return s
