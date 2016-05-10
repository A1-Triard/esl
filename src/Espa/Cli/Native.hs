module Espa.Cli.Native where

#include <haskell>
import Paths_esp_assembler
import Data.Tes3.Disassembler

espSuffix :: String
espSuffix = ".esp"

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

espaOptions :: [String] -> (EspaOptions, [FilePath], [String])
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
      | optShowVersion options = putStrLn $ "espa " ++ showVersion version
      | not $ null errors = hPutStrLn stderr $ concat errors ++ espaUsageErrorFooter
      | optDisassembly options = forM_ (if null names then ["-"] else names) $ printErrors espaDisassemblyErrorText . espaDisassembly
      | otherwise = forM_ (if null names then ["-"] else names) $ printErrors espaAssemblyErrorText . espaAssembly
      
printErrors :: (e -> String) -> ExceptT e IO () -> IO ()
printErrors error_text action = do
  result <- runExceptT action
  case result of
    Left e -> handle (\x -> let _ = x :: IOError in return ()) $ hPutStrLn stderr $ error_text e
    Right _ -> return ()
    
class IOErrorHost a where
  fromIOError :: IOError -> a

tryIO' :: IOErrorHost e => IO a -> ExceptT e IO a
tryIO' = withExceptT fromIOError . tryIO

data EspDisassemblyError
  = EspDisassemblyIOError IOError
  | EspDisassemblyBadSuffix FilePath
  
instance IOErrorHost EspDisassemblyError where
  fromIOError = EspDisassemblyIOError

espaDisassemblyErrorText :: EspDisassemblyError -> String
espaDisassemblyErrorText (EspDisassemblyBadSuffix name) = name ++ ": Filename has an unknown suffix, skipping"
espaDisassemblyErrorText (EspDisassemblyIOError e) = ioeGetErrorString e

getDisassembliedFileName :: FilePath -> Either EspDisassemblyError FilePath
getDisassembliedFileName name
  | endswith espSuffix name = Right $ take (length name - length espSuffix) name
  | otherwise = Left $ EspDisassemblyBadSuffix name

espaDisassembly :: FilePath -> ExceptT EspDisassemblyError IO ()
espaDisassembly name = do
  output_name <- hoistEither $ getDisassembliedFileName name
  tryIO' $ putStrLn $ "disassembly " ++ name ++ " -> " ++ output_name

espaAssemblyErrorText :: IOError -> String
espaAssemblyErrorText _ = "error"

espaAssembly :: FilePath -> ExceptT IOError IO ()
espaAssembly name = tryIO $ putStrLn $ "assembly " ++ name
