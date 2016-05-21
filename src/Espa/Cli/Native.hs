module Espa.Cli.Native where

#include <haskell>
import Paths_esp_assembler
import Control.Error.Extensions
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
      | optDisassembly options = forM_ (filenames names) $ printErrors espaDisassemblyErrorText . espaDisassembly (verboser options)
      | otherwise = forM_ (filenames names) $ printErrors espaAssemblyErrorText . espaAssembly (verboser options)
    filenames names = if null names then ["-"] else names
    verboser options
      | optVerbose options = handle (\x -> let _ = x :: IOError in return ()) . hPutStrLn stderr
      | otherwise = \_ -> return ()

type Verboser = String -> IO ()

printErrors :: (e -> String) -> ExceptT e IO () -> IO ()
printErrors error_text action = do
  result <- runExceptT action
  case result of
    Left e -> handle (\x -> let _ = x :: IOError in return ()) $ hPutStrLn stderr $ error_text e
    Right _ -> return ()
    
espaDisassemblyErrorText :: IOError -> String
espaDisassemblyErrorText e
  | isUserError e = ioeGetErrorString e
  | isDoesNotExistError e = fromMaybe "" (ioeGetFileName e) ++ ": No such file or directory"
  | otherwise = ioeGetErrorString e

getDisassembliedFileName :: FilePath -> Either IOError FilePath
getDisassembliedFileName name
  | name == "-" = Right "-"
  | endswith espSuffix name = Right $ take (length name - length espSuffix) name
  | otherwise = Left $ userError $ name ++ ": Filename has an unknown suffix, skipping"
  
withBinaryInputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withBinaryInputFile "-" action = action stdin
withBinaryInputFile name action = bracketE (tryIO $ openBinaryFile name ReadMode) (tryIO . hClose) action

withTextOutputFile :: FilePath -> (Handle -> ExceptT IOError IO a) -> ExceptT IOError IO a
withTextOutputFile "-" action = action stdout
withTextOutputFile name action = bracketE (tryIO $ openFile name WriteMode) (tryIO . hClose) action

handleT3Error :: FilePath -> String -> IOError
handleT3Error name e = userError $ showString name $ showString ": " e

espaDisassembly :: Verboser -> FilePath -> ExceptT IOError IO ()
espaDisassembly verbose name = do
  output_name <- hoistEither $ getDisassembliedFileName name
  tryIO $ verbose $ name ++ " -> " ++ output_name
  r <-
    withBinaryInputFile name $ \input -> do
      withTextOutputFile output_name $ \output -> do
        runConduit $ (N.sourceHandle input =$= disassembly) `fuseUpstream` N.sinkHandle output
  case r of
    Right _ -> return ()
    Left (offset, err) -> do
      tryIO $ removeFile output_name
      case err of
        Right e -> throwE $ userError $ name ++ ": " ++ replace "{0}" (showHex offset "h") e
        Left e -> throwE $ userError $ name ++ ": " ++ "Internal error: " ++ showHex offset "h: " ++ e

espaAssemblyErrorText :: IOError -> String
espaAssemblyErrorText _ = "error"

espaAssembly :: Verboser -> FilePath -> ExceptT IOError IO ()
espaAssembly v name = tryIO $ v $ "assembly " ++ name
