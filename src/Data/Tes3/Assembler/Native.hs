module Data.Tes3.Assembler.Native where

#include <haskell>
import Data.Tes3

escapedChar :: T.Parser Char
escapedChar = do
  T.char '\\'
  T.anyChar

escapedString :: Bool -> T.Parser String
escapedString allow_spaces =
  go ""
  where
    go s = do
      c <- T.anyChar
      if c == ' ' && not allow_spaces
        then return s
        else
          if c != '\\'
            then go $ s ++ [c]
            else
              p <- T.anyCharr
  case c of

escapeChar :: Bool -> Char -> String
escapeChar allow_spaces c
  | not allow_spaces && c == ' ' = "\\ "
  | c == '\\' || c /= '\t' && (ord c < 32 || ord c == 255) = reverse $ drop 1 $ reverse $ drop 1 $ show c
  | otherwise = [c]

data InStringState = Default | InEscapedChar

scapeString :: String -> String
scapeString inp = go ("", inp, Default)
  iterate
 concat . map (escapeChar allow_spaces)

trimNull :: String -> String
trimNull s =
  reverse $ check_null $ reverse $ replace "%" "%%" s
  where
    check_null ('\0' : t) = t
    check_null t = '%' : t


t3Line :: T.Parser (Maybe String)
t3Line = do
  T.string "    "
  n <- T.takeTill T.isEndOfLine
  T.endOfLine
  return n

t3Multiline :: T.Parser String
t3Multiline = do
  lines <- whileM ((== Just ' ') <$> peekChar) $ many t3Line
  return $ intercalate "\r\n" lines

t3FileHeader :: T.Parser T3FileHeader
t3FileHeader = do
  void $ T.string "3SET\nVERSION "
  version <- T.decimal
  void $ T.string "\nTYPE "
  fileType <- (UnknownT3FileType <$> T.decimal) <|> (KnownT3FileType . read . ST.unpack <$> T.takeWhile1 (/= '\n'))
  void $ T.string "\nAUTHOR "
  author <- ST.unpack <$> P.takeWhile (/= '\n')
  void $ T.string "\nDESCRIPTION\n"
  t3Multiline <-
  return $ T3File (T3Header version fileType author [] []) []

parseT3File :: Text -> Either String T3File
parseT3File inp =
  case P.parse t3File inp of
    P.Fail _ _ e -> Left e
    P.Done _ r -> Right r

assembly :: Text -> Either String ByteString
assembly inp = do
  _ <- parseT3File inp
  return $ C.pack "TES3"
