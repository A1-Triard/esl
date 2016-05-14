module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3
  
labelEOF :: String -> Get a -> Get a
labelEOF s = label $ showString "REPLACE/not enough bytes/" s

getError :: ByteOffset -> String -> String
getError offset s =
  replace "BYTES" (showHex offset "") $ process $ viewL $ lines s
  where
    process Nothing = ""
    process (Just (e, rules)) = foldl apply e rules
    apply e ('R' : 'E' : 'P' : 'L' : 'A' : 'C' : 'E' : '/' : r) =
      case splitOn "/" r of
        (a : b : []) -> if e == a then b else e
        _ -> e
    apply e _  = e

expect :: (Show a, Eq a) => a -> Get a -> Get ()
expect expected getter = do
  offset <- bytesRead
  actual <- getter
  if actual /= expected
    then fail $ showHex offset $ showString ": " $ shows expected $ showString " expected, but " $ shows actual " provided."
    else return ()

sign :: Get T3Sign
sign = t3SignNew <$> getWord32le

size :: Get Word32
size = getWord32le

gap :: Get ()
gap = expect 0 getWord64le

data T3Record body = T3Record T3Sign body

expectRecord :: T3Sign -> Get body -> Get (T3Record body)
expectRecord s body = do
  expect s sign
  z <- size
  gap
  b <- isolate (fromIntegral z) body
  return $ T3Record s b
  
skipAll :: Get ()
skipAll = do
  whileM_ (not <$> isEmpty) $ skip 1
  return ()
  
file :: Get ()
file = labelEOF "BYTES: unexpected end of file." $ do
  void $ expectRecord (T3Mark TES3) skipAll
  skipAll

disassembly :: ByteString -> Either String String
disassembly b =
  case pushEndOfInput $ runGetIncremental file `pushChunks` b of
    Fail _ offset e -> Left $ getError offset e
    Done (SB.null -> False) offset _ -> Left $ showHex offset ": end of file expected."
    Done _ _ _ -> Right "ESP\n"
    _ -> Left "Internal error."
