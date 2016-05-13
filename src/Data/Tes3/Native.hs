module Data.Tes3.Native where

#include <haskell>

data T3Mark
  = TES3 | FNAM | NAME | SCPT | CONT
  | MISC | NPC_
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data T3Sign = T3Mark T3Mark | T3Sign Word32 deriving Eq

instance Show T3Sign where
  show (T3Mark m) = show m
  show (T3Sign s) =
    let (a, b, c, d) = getBytes s in
    [chr $ fromIntegral a, chr $ fromIntegral b, chr $ fromIntegral c, chr $ fromIntegral d]

combineBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
combineBytes a b c d
  =  (fromIntegral a)
  .|. (shift (fromIntegral b) 8)
  .|. (shift (fromIntegral c) 16)
  .|. (shift (fromIntegral d) 24)
  
getBytes :: Word32 -> (Word8, Word8, Word8, Word8)
getBytes w =
  ( fromIntegral $ w .&. 0xFF
  , fromIntegral $ (.&. 0xFF) $ shift w (-8)
  , fromIntegral $ (.&. 0xFF) $ shift w (-16)
  , fromIntegral $ (.&. 0xFF) $ shift w (-24)
  )

t3MarkValueSlow :: T3Mark -> Word32
t3MarkValueSlow mark =
  calc (show mark)
  where
    calc [a, b, c, d] = combineBytes (fromIntegral $ ord a) (fromIntegral $ ord b) (fromIntegral $ ord c) (fromIntegral $ ord d)
    calc _ = error "T3Mark"

t3MarkValues :: S.Map T3Mark Word32
t3MarkValues = SM.fromList [(m, t3MarkValueSlow m) | m <- [minBound .. maxBound]]

t3MarkValue :: T3Mark -> Word32
t3MarkValue m = fromMaybe 0 $ SM.lookup m t3MarkValues

t3SignValue :: T3Sign -> Word32
t3SignValue (T3Mark m) = t3MarkValue m
t3SignValue (T3Sign s) = s

t3MarkNews :: S.Map Word32 T3Mark
t3MarkNews = SM.fromList [(t3MarkValue m, m) | m <- [minBound .. maxBound]]

t3MarkNew :: Word32 -> Maybe T3Mark
t3MarkNew w = SM.lookup w t3MarkNews

t3SignNew :: Word32 -> T3Sign
t3SignNew w = fromMaybe (T3Sign w) $ T3Mark <$> t3MarkNew w
