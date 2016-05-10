module Espa.Cli.Native where

#include <haskell>
import Paths_esp_assembler
import Data.Tes3.Disassembler

espaHelpHeader :: String
espaHelpHeader
  =  "Usage: espa [OPTION]... [FILE]...\n"
  ++ "Assembly or disassembly FILEs in the .esp format.\n"

espaHelpFooter :: String
espaHelpFooter
  =  "\nWith no FILE, or when FILE is -, read standard input.\n"
  ++ "\nReport bugs to <internalmike@gmail.com> (in English or Russian)."
  ++ "\nESP Assembler home page: <https://github.com/A1-Triard/esp-assembler>"

espaUsageErrorFooter :: String
espaUsageErrorFooter
  = "Try `espa --help' for more information."

data EspaOptions = EspaOptions
  { optDisassembly :: Bool
  , optShowVersion :: Bool
  , optShowHelp :: Bool
  , optVerbose :: Bool
  }

defaultEspaOptions :: EspaOptions
defaultEspaOptions = EspaOptions
  { optDisassembly = False
  , optShowVersion = False
  , optShowHelp = False
  , optVerbose = False
  }
  
espaOptionsDescr :: [OptDescr (EspaOptions -> EspaOptions)]
espaOptionsDescr =
  [ Option ['d'] ["disassembly"] (NoArg (\o -> o {optDisassembly = True})) "force disassembliation"
  , Option ['V'] ["version"] (NoArg (\o -> o {optShowVersion = True})) "display the version number and exit"
  , Option ['h'] ["help"] (NoArg (\o -> o {optShowHelp = True})) "display this help and exit"
  , Option ['v'] ["verbose"] (NoArg (\o -> o {optVerbose = True})) "be verbose"
  ]

espaOptions :: [String] -> (EspaOptions, [String], [String])
espaOptions args =
  let (options, names, errors) = getOpt Permute espaOptionsDescr args in
  (foldl (flip id) defaultEspaOptions options, names, errors)

espa :: IO ()
espa = do
  args <- getArgs
  process_options $ espaOptions args
  where
    process_options (options, names, errors)
      | optShowHelp options = putStrLn $ usageInfo espaHelpHeader espaOptionsDescr ++ espaHelpFooter
      | not $ null errors = hPutStrLn stderr $ concat errors ++ espaUsageErrorFooter
      | optShowVersion options = putStrLn $ "espa " ++ showVersion version
      | optDisassembly options = espaDisassembly names
      | otherwise = espaAssembly names

espaDisassembly :: [String] -> IO ()
espaDisassembly names = putStrLn $ "disassembly " ++ concat names

espaAssembly :: [String] -> IO ()
espaAssembly names = putStrLn $ "assembly " ++ concat names
