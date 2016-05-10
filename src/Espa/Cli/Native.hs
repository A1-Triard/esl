module Espa.Cli.Native where

#include <haskell>
import Data.Tes3.Disassembler

espa :: IO ()
espa = putStrLn "someFunc"
