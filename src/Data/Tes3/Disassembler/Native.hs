module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Get

escapeChar :: Bool -> Char -> String
escapeChar allow_spaces c
  | not allow_spaces && c == ' ' = "\\ "
  | c == '\\' || c /= '\t' && (ord c < 32 || ord c == 255) = reverse $ drop 1 $ reverse $ drop 1 $ show c
  | otherwise = [c]

escapeString :: Bool -> String -> String
escapeString allow_spaces = concat . map (escapeChar allow_spaces)

trimNull :: String -> String
trimNull s =
  reverse $ check_null $ reverse $ replace "%" "%%" s
  where
    check_null ('\0' : t) = t
    check_null t = '%' : t

writeT3Header :: T3Header -> String
writeT3Header (T3Header version file_type author description refs)
  =  "VERSION " ++ show version ++ "\n"
  ++ "TYPE " ++ show file_type ++ "\n"
  ++ "AUTHOR " ++ escapeString True author ++ "\n"
  ++ "DESCRIPTION\n" ++ (intercalate "\n" $ map (("    " ++) . escapeString True) description) ++ "\n"
  ++ concat [(escapeString False $ trimNull n) ++ " " ++ show z ++ "\n" | (T3FileRef n z) <- refs]

writeT3Field :: T3Field -> String
writeT3Field (T3BinaryField sign d) = show sign ++ " " ++ C.unpack (encode d) ++ "\n"
writeT3Field (T3StringField sign s) = show sign ++ " " ++ (escapeString True $ trimNull s) ++ "\n"
writeT3Field (T3FixedStringField sign s) = show sign ++ " " ++ escapeString True s ++ "\n"
writeT3Field (T3MultilineField sign t) = show sign ++ "\n" ++ (intercalate "\n" $ map (("    " ++) . escapeString True) t) ++ "\n"
writeT3Field (T3RefField sign z n) = show sign ++ " " ++ show z ++ " " ++ escapeString True n ++ "\n"

writeT3Record :: T3Record -> String
writeT3Record (T3Record sign gap fields)
  =  "\n" ++ show sign ++ (if gap == 0 then "" else " " ++ show gap) ++ "\n"
  ++ concat [writeT3Field f | f <- fields]

writeT3File :: T3File -> String
writeT3File (T3File header records)
  =  "3SET\n"
  ++ writeT3Header header
  ++ concat [writeT3Record r | r <- records]

disassembly :: ByteString -> Either String String
disassembly b = do
  f <- runGetT3File b
  return $ writeT3File f
