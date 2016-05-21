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

writeT3Header :: T3FileHeader -> Text
writeT3Header (T3FileHeader version file_type author description refs)
  =  "VERSION " <> T.pack (show version) <> "\n"
  <> "TYPE " <> T.pack (show file_type) <> "\n"
  <> "AUTHOR " <> T.pack (escapeString True author) <> "\n"
  <> "DESCRIPTION\n" <> (T.intercalate "\n" $ map (("    " <>) . T.pack . escapeString True) description) <> "\n"
  <> T.concat [T.pack (escapeString False $ trimNull n) <> " " <> T.pack (show z) <> "\n" | (T3FileRef n z) <- refs]

writeT3Field :: T3Field -> Text
writeT3Field (T3BinaryField sign d) = T.pack (show sign) <> " " <> T.pack (C.unpack (encode d)) <> "\n"
writeT3Field (T3StringField sign s) = T.pack (show sign) <> " " <> T.pack (escapeString True $ trimNull s) <> "\n"
writeT3Field (T3FixedStringField sign s) = T.pack (show sign) <> " " <> T.pack (escapeString True s) <> "\n"
writeT3Field (T3MultilineField sign t) = T.pack (show sign) <> "\n" <> (T.intercalate "\n" $ map (("    " <>). T.pack . escapeString True) t) <> "\n"
writeT3Field (T3RefField sign z n) = T.pack (show sign) <> " " <> T.pack (show z) <> " " <> T.pack (escapeString True n) <> "\n"

writeT3Record :: T3Record -> Text
writeT3Record (T3Record sign gap fields)
  =  "\n" <> T.pack (show sign) <> (if gap == 0 then "" else " " <> T.pack (show gap)) <> "\n"
  <> T.concat [writeT3Field f | f <- fields]

conduitGet1 :: Monad m => Get e a -> ByteOffset -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, a))
conduitGet1 g base_offset = do
  go $ runGetIncremental base_offset g
  where
    go (Partial p) = do
      inp <- await
      go $ p inp
    go (Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, result)
    go (Fail _ offset err) = do
      return $ Left (offset, err)

conduitGetN :: (Monad m, Num n) => Get e a -> (ByteOffset, n) -> ConduitM S.ByteString a m (Either (ByteOffset, Either String e) (ByteOffset, n))
conduitGetN g (base_offset, n) = do
  go $ runGetIncremental base_offset g
  where
    go (Partial p) = do
      inp <- await
      go $ p inp
    go (Done unused offset result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right (offset, n + 1)
    go (Fail _ offset err) = do
      return $ Left (offset, err)

conduitRepeat :: Monad m => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
conduitRepeat a0 produce =
  go a0
  where
    go an = do
      end <- N.null
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right an_1 -> go an_1

conduitRepeatE :: (Monad m, MonoFoldable seq) => a -> (a -> ConduitM seq r m (Either e a)) -> ConduitM seq r m (Either e a)
conduitRepeatE a0 produce =
  go a0
  where
    go an = do
      end <- N.nullE
      if end
        then return $ Right an
        else do
          p <- produce an
          case p of
            Left err -> return $ Left err
            Right an_1 -> go an_1

disassembly :: Monad m => ConduitM S.ByteString Text m (Either (ByteOffset, Either String String) ())
disassembly = runExceptT $ do
  (h, _) <- (hoistEither =<<) $ lift $ mapOutput (const "3SET\n") $ conduitGet1 getT3FileSignature 0
  (r, (_, items_count)) <- (hoistEither =<<) $ lift $ mapOutput (writeT3Header . fst) $ conduitGet1 getT3FileHeader h
  (f, n) <- (hoistEither =<<) $ lift $ mapOutput writeT3Record $ conduitRepeatE (r, 0) $ conduitGetN getT3Record
  if n /= items_count
    then hoistEither $ Left (f, Right $ "Records count mismatch: " ++ show items_count ++ " expected, but " ++ show n ++ " readed.")
    else return ()
