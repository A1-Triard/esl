module Data.Tes3.Assembler.Native where

#include <haskell>
import Data.Tes3
import Data.Tes3.Parser

putFileSignature :: 

conduitParser1 :: Monad m => Parser a -> ConduitM Text a m (Either String a)
conduitParser1 p = do
  go $ Tp.parse p ""
  where
    go (Partial p) = do
      inp <- await
      go $ p inp
    go (Done unused result) = do
      yield result
      if SB.null unused
        then return ()
        else leftover unused
      return $ Right result
    go (Fail _ _ err) = do
      return $ Left err

assembly :: Monad m => ConduitM Text ByteString m (Either String ())
assembly = runExceptT $ do
  (h, _) <- (hoistEither =<<) $ lift $ mapOutput (const "3SET\n") $ conduitParser1 pT3FileSignature
  return $ C.pack "TES3"
