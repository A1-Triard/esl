module Data.Tes3.Disassembler.Native where

#include <haskell>

data T3Error = UnexpectedEOF

disassembly :: ByteString -> Either T3Error String
disassembly _ = Right "ESP"
