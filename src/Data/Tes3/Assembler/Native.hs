module Data.Tes3.Assembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Parser

sign :: T3Sign -> ByteString
sign = runPut . putWord32le . t3SignValue

size :: ByteString -> ByteString
size = runPut . putWord32le . fromIntegral . B.length

gap :: Word64 -> ByteString
gap = runPut . putWord64le

tail :: ByteString -> Word32 -> ByteString
tail b n = runPut (foldl (>>) (return ()) $ replicate (fromIntegral $ n - fromIntegral (B.length b)) (putWord8 0))

putT3FileSignature :: ByteString
putT3FileSignature = sign (T3Mark TES3)

putT3Field :: T3Sign -> T3Field -> ByteString
putT3Field _ (T3BinaryField s b) = sign s <> size b <> b
putT3Field _ (T3StringField s t) =
  let b = t3StringValue t in
  sign s <> size b <> b
putT3Field record_sign (T3FixedStringField s t) =
  let b = t3StringValue t in
  let (T3FixedString n) = t3FieldType record_sign s in
  sign s <> runPut (putWord32le n) <> b <> tail b n
putT3Field _ (T3MultilineField s t) =
  let b = t3StringValue $ T.intercalate "\r\n" t in
  sign s <> size b <> b
putT3Field _ (T3MultiStringField s t) =
  let b = t3StringValue $ T.intercalate "\0" t in
  sign s <> size b <> b
putT3Field _ (T3RefField s n t) =
  let b = t3StringValue t in
  sign s <> runPut (putWord32le 36) <> runPut (putWord32le n) <> b <> tail b 32

putT3Record :: T3Record -> ByteString
putT3Record (T3Record s g fields) =
  let b = foldl (<>) B.empty [putT3Field s f | f <- fields] in
  sign s <> size b <> gap g <> b

t3FileRef :: T3FileRef -> ByteString
t3FileRef (T3FileRef n z) =
  let mast = t3StringValue n in
  let dat = runPut $ putWord64le z in
  sign (T3Mark MAST) <> size mast <> mast <> sign (T3Mark DATA) <> size dat <> dat

putT3FileHeader :: T3FileHeader -> ByteString
putT3FileHeader (T3FileHeader version file_type author descr refs) =
  let v = runPut $ putWord32le version in
  let f = runPut $ putWord32le $ t3FileTypeValue file_type in
  let a = t3StringValue author in
  let d = t3StringValue $ T.intercalate "\r\n" descr in
  let r = foldl (<>) B.empty [t3FileRef ref | ref <- refs] in
  let items_count_placeholder = runPut $ putWord32le 0 in
  let tes3 = sign (T3Mark HEDR) <> runPut (putWord32le 300) <> v <> f <> a <> tail a 32 <> d <> tail d 256 <> items_count_placeholder <> r in
  size tes3 <> gap 0 <> tes3

awaitE :: Monad m => ConduitM S.Text a m S.Text
awaitE = do
  maybe_inp <- await
  case maybe_inp of
    Nothing -> return ST.empty
    Just (ST.null -> True) -> awaitE
    Just inp -> return inp

conduitParser1 :: Monad m => T.Parser a -> ConduitM S.Text a m (Either String a)
conduitParser1 parser = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      inp <- awaitE
      go $ p inp
    go (TP.Done unused result) = do
      yield result
      if ST.null unused
        then return ()
        else leftover unused
      return $ Right result
    go (TP.Fail _ _ err) = do
      return $ Left err

conduitParserN :: (Monad m, Num n) => T.Parser (Maybe a) -> n -> ConduitM S.Text a m (Either String (Maybe n))
conduitParserN parser n = do
  go $ TP.parse parser ""
  where
    go (TP.Partial p) = do
      inp <- awaitE
      go $ p inp
    go (TP.Done unused result) = do
      case result of
        Nothing -> return ()
        Just r -> yield r
      if ST.null unused
        then return ()
        else leftover unused
      return $ Right $ if isJust result then Just (n + 1) else Nothing
    go (TP.Fail _ c err) = do
      return $ Left $ err ++ " (" ++ intercalate " -> " c ++ ")"

conduitRepeat :: Monad m => a -> (a -> ConduitM seq r m (Either e (Maybe a))) -> ConduitM seq r m (Either e a)
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
            Right (Just an_1) -> go an_1
            Right Nothing -> return $ Right an

conduitRepeatE :: (Monad m, MonoFoldable seq) => a -> (a -> ConduitM seq r m (Either e (Maybe a))) -> ConduitM seq r m (Either e a)
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
            Right (Just an_1) -> go an_1
            Right Nothing -> return $ Right an

pMaybeT3Record :: T.Parser (Maybe T3Record)
pMaybeT3Record = do
  Tp.endOfLine
  c <- Tp.peekChar
  if c == Just '#'
    then return Nothing
    else Just <$> pT3Record

pRecordsNumber :: T.Parser Word32
pRecordsNumber = do
  void $ Tp.char '#'
  void $ Tp.char ' '
  n <- Tp.decimal
  Tp.endOfLine
  return n

assembly :: Monad m => ConduitM S.Text ByteString m (Either String (T3FileType, Word32))
assembly = runExceptT $ do
  (hoistEither =<<) $ lift $ mapOutput (const putT3FileSignature) $ conduitParser1 (pT3FileSignature <?> "S")
  T3FileHeader _ file_type _ _ _ <- (hoistEither =<<) $ lift $ mapOutput putT3FileHeader $ conduitParser1 (pT3FileHeader <?> "H")
  n <- (hoistEither =<<) $ lift $ mapOutput putT3Record $ conduitRepeatE 0 $ conduitParserN (pMaybeT3Record <?> "R")
  items_count <- (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.option n pRecordsNumber <?> "RN")
  if items_count < n
    then hoistEither $ Left $ "Records count mismatch: no more than " ++ show items_count ++ " expected, but " ++ show n ++ " readed."
    else return ()
  (hoistEither =<<) $ lift $ mapOutput (const B.empty) $ conduitParser1 (Tp.endOfInput <?> "EOF")
  return (file_type, items_count)
