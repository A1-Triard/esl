module Data.Tes3.Assembler.Native where

#include <haskell>
import Data.Tes3

t3File :: P.Parser T3File
t3File = do
  _ <- P.string "3TES\nVERSION "
  version <- P.decimal
  _ <- P.string "\nTYPE "
  fileType <- (UnknownT3FileType <$> P.decimal) <|> (KnownT3FileType . read . ST.unpack <$> P.takeWhile1 (/= '\n'))
  _ <- P.string "\nAUTHOR "
  author <- ST.unpack <$> P.takeWhile (/= '\n')
  _ <- P.string "\nDESCRIPTION\n"
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
