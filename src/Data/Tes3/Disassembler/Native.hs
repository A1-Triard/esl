module Data.Tes3.Disassembler.Native where

#include <haskell>
import Data.Tes3

data T3Error = UnexpectedEOF

sign :: Get T3Sign
sign = t3SignNew <$> getWord32le

disassembly :: ByteString -> Either T3Error String
disassembly _ = Right "ESP\n"
