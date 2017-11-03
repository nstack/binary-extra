module Data.Binary.Extra.Put
  ( module Data.Binary.Extra.Put
  , module Data.Binary.Extra.Endian
  ) where
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.State hiding (put)
import           Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import qualified Data.Binary.Put as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Functor
import           Data.Functor.Identity
import           Data.Int
import           Data.Semigroup
import           Data.Word

import Data.Binary.Extra.Endian

data SPair a b = SPair { sFst :: !a, sSnd :: !b }
  deriving (Eq, Ord, Show)

sPairToTuple :: SPair a b -> (a, b)
sPairToTuple (SPair a b) = (a, b)

-- newtype to get Monoid instance
newtype PutT m a = PutT { runPutM' :: StateT (SPair Integer Builder) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)

deriving instance MonadReader r m => MonadReader r (PutT m)

pattern PutM :: State (SPair Integer Builder) a -> PutM a
pattern PutM a = PutT a

type PutM = PutT Identity

instance Semigroup (PutM ()) where
  PutT a <> PutT b = PutT . state $ \s -> let ((), s1) = runState a s
                                          in runState b s1

instance Monoid (PutM ()) where
  mempty = PutT $ pure mempty
  mappend = (<>)

type Put = PutM ()

runPut :: Put -> (Integer, Builder)
runPut (PutT p) = sPairToTuple $ execState p (SPair 0 mempty)

runPutT :: Monad m => PutT m () -> m (Integer, Builder)
runPutT (PutT p) = sPairToTuple <$> execStateT p (SPair 0 mempty)

runPutWith :: Integer -> Put -> (Integer, Builder)
runPutWith start (PutT p) = sPairToTuple $ execState p (SPair start mempty)

execPut :: PutM a -> Builder
execPut (PutT p) = sSnd $ execState p (SPair 0 mempty)

execPutT :: Monad m => PutT m a -> m Builder
execPutT (PutT p) = sSnd <$> execStateT p (SPair 0 mempty)

evalPut :: PutM a -> P.PutM a
evalPut (PutT a) = let (r, SPair _ b) = runState a (SPair 0 mempty) in P.putBuilder b $> r

incrBW :: Monad m => Integer -> PutT m ()
incrBW i = PutT . modify' $ \(SPair x b) -> SPair (i + x) b
{-# INLINE incrBW #-}

liftIncr :: Monad m => Integer -> P.Put -> PutT m ()
liftIncr i p =
  PutT . modify' $ \(SPair j b) -> SPair (i + j) $ b `mappend` P.execPut p
{-# INLINE liftIncr #-}

getBytesWritten :: Monad m => PutT m Integer
getBytesWritten = PutT $ gets sFst
{-# INLINE getBytesWritten #-}

putBuilder :: Monad m => Builder -> PutT m ()
putBuilder b =
  let len = LBS.length $ Builder.toLazyByteString b
  in PutT . modify' $ \(SPair i x) -> SPair (i + fromIntegral len) $ x <> b
{-# INLINE putBuilder #-}

put :: Monad m => P.Put -> PutT m ()
put = putBuilder . P.execPut
{-# INLINE put #-}

align :: Monad m => Int -> PutT m ()
align x
  | x == 0 = return ()
  | otherwise = do
    let x' = fromIntegral x
    c <- getBytesWritten
    case c `mod` x' of
      0 -> return ()
      r ->
        let n = x' - r
        in liftIncr n . replicateM_ (fromInteger n) $ P.putWord8 0
{-# INLINE align #-}

flush :: Monad m => PutT m ()
flush = PutT . modify' $ \(SPair i x) -> SPair i $ x <> P.execPut P.flush
{-# INLINE flush #-}

-- primitives

putWord8 :: Monad m => Word8 -> PutT m ()
putWord8 = liftIncr 1 . P.putWord8
{-# INLINE putWord8 #-}

putInt8 :: Monad m => Int8 -> PutT m ()
putInt8 = liftIncr 1 . P.putInt8
{-# INLINE putInt8 #-}

putByteString :: Monad m => BS.ByteString -> PutT m ()
putByteString bs = liftIncr (fromIntegral $ BS.length bs) $ P.putByteString bs
{-# INLINE putByteString #-}

putLazyByteString :: Monad m => LBS.ByteString -> PutT m ()
putLazyByteString bs = liftIncr (fromIntegral $ LBS.length bs) $ P.putLazyByteString bs
{-# INLINE putLazyByteString #-}

putShortByteString :: Monad m => SBS.ShortByteString -> PutT m ()
putShortByteString bs = liftIncr (fromIntegral $ SBS.length bs) $ P.putShortByteString bs
{-# INLINE putShortByteString #-}

-- big-endian primitives

putWord16be :: Monad m => Word16 -> PutT m ()
putWord16be = liftIncr 2 . P.putWord16be
{-# INLINE putWord16be #-}

putWord32be :: Monad m => Word32 -> PutT m ()
putWord32be = liftIncr 4 . P.putWord32be
{-# INLINE putWord32be #-}

putWord64be :: Monad m => Word64 -> PutT m ()
putWord64be = liftIncr 8 . P.putWord64be
{-# INLINE putWord64be #-}

putInt16be :: Monad m => Int16 -> PutT m ()
putInt16be = liftIncr 2 . P.putInt16be
{-# INLINE putInt16be #-}

putInt32be :: Monad m => Int32 -> PutT m ()
putInt32be = liftIncr 4 . P.putInt32be
{-# INLINE putInt32be #-}

putInt64be :: Monad m => Int64 -> PutT m ()
putInt64be = liftIncr 8 . P.putInt64be
{-# INLINE putInt64be #-}

putFloatbe :: Monad m => Float -> PutT m ()
putFloatbe = liftIncr 4 . P.putFloatbe
{-# INLINE putFloatbe #-}

putDoublebe :: Monad m => Double -> PutT m ()
putDoublebe = liftIncr 8 . P.putDoublebe
{-# INLINE putDoublebe #-}

-- little-endian primitives

putWord16le :: Monad m => Word16 -> PutT m ()
putWord16le = liftIncr 2 . P.putWord16le
{-# INLINE putWord16le #-}

putWord32le :: Monad m => Word32 -> PutT m ()
putWord32le = liftIncr 4 . P.putWord32le
{-# INLINE putWord32le #-}

putWord64le :: Monad m => Word64 -> PutT m ()
putWord64le = liftIncr 8 . P.putWord64le
{-# INLINE putWord64le #-}

putInt16le :: Monad m => Int16 -> PutT m ()
putInt16le = liftIncr 2 . P.putInt16le
{-# INLINE putInt16le #-}

putInt32le :: Monad m => Int32 -> PutT m ()
putInt32le = liftIncr 4 . P.putInt32le
{-# INLINE putInt32le #-}

putInt64le :: Monad m => Int64 -> PutT m ()
putInt64le = liftIncr 8 . P.putInt64le
{-# INLINE putInt64le #-}

putFloatle :: Monad m => Float -> PutT m ()
putFloatle = liftIncr 4 . P.putFloatle
{-# INLINE putFloatle #-}

putDoublele :: Monad m => Double -> PutT m ()
putDoublele = liftIncr 8 . P.putDoublele
{-# INLINE putDoublele #-}

-- endian polymorphic functions

wEndian :: MonadReader Endian m => Put -> Put -> PutT m ()
wEndian a b = ask >>= endian (hoist generalize a) (hoist generalize b)

wEndian' :: MonadReader Endian m => (a -> Put) -> (a -> Put) -> a -> PutT m ()
wEndian' a b c = wEndian (a c) (b c)

putWord16 :: MonadReader Endian m => Word16 -> PutT m ()
putWord16 = wEndian' putWord16le putWord16be

putWord32 :: MonadReader Endian m => Word32 -> PutT m ()
putWord32 = wEndian' putWord32le putWord32be

putWord64 :: MonadReader Endian m => Word64 -> PutT m ()
putWord64 = wEndian' putWord64le putWord64be

putInt16 :: MonadReader Endian m => Int16 -> PutT m ()
putInt16 = wEndian' putInt16le putInt16be

putInt32 :: MonadReader Endian m => Int32 -> PutT m ()
putInt32 = wEndian' putInt32le putInt32be

putInt64 :: MonadReader Endian m => Int64 -> PutT m ()
putInt64 = wEndian' putInt64le putInt64be

putFloat :: MonadReader Endian m => Float -> PutT m ()
putFloat = wEndian' putFloatle putFloatbe

putDouble :: MonadReader Endian m => Double -> PutT m ()
putDouble = wEndian' putDoublele putDoublebe

-- unicode
-- here we just run the resulting putter to get outputted length
-- as varies with encoding of each char

putCharUtf8 :: Monad m => Char -> PutT m ()
putCharUtf8 = put . P.putCharUtf8
{-# INLINE putCharUtf8 #-}

putStringUtf8 :: Monad m => String -> PutT m ()
putStringUtf8 = put . P.putStringUtf8
{-# INLINE putStringUtf8 #-}
