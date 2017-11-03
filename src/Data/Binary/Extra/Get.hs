module Data.Binary.Extra.Get
  ( runGetIncrementalWithOffset
  , decodeWithBuilder
  , GetE
  , runGetE
  , getWord16
  , getWord32
  , getWord64
  , getInt16
  , getInt32
  , getInt64
  , getFloat
  , getDouble
  , module G
  , module Data.Binary.Extra.Endian
  ) where
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import           Data.Binary.Get as G
import qualified Data.Binary.Get.Internal as I
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import Data.Binary.Extra.Endian

runGetIncrementalWithOffset :: G.ByteOffset -> G.Get a -> G.Decoder a
runGetIncrementalWithOffset offset = calculateOffset offset . I.runGetIncremental

calculateOffset :: G.ByteOffset -> I.Decoder a -> G.Decoder a
calculateOffset off r0 = go r0 off
  where
    go r !acc =
      case r of
        I.Done inp a -> G.Done inp (acc - fromIntegral (B.length inp)) a
        I.Fail inp s -> G.Fail inp (acc - fromIntegral (B.length inp)) s
        I.Partial k ->
          G.Partial $ \ms ->
            case ms of
              Nothing -> go (k Nothing) acc
              Just i -> go (k ms) (acc + fromIntegral (B.length i))
        I.BytesRead unused k -> go (k $! (acc - unused)) acc

-- | Modify a 'Decoder' so that it also returns a 'B.Builder' that will
-- reconstruct the original input.
decodeWithBuilder :: Decoder a -> Decoder (B.Builder, a)
decodeWithBuilder = go 0 mempty mempty
  where
    go !sz !builder !final (Done a b c) =
      let newbld = builder <> B.byteString (B.take (fromIntegral b - sz) final)
      in Done a b (newbld, c)
    go !sz !builder !final (Partial k) =
      Partial $ \bs ->
                  let newsz = sz + B.length final
                      newbld = builder <> B.byteString final
                      newfin = fromMaybe mempty bs
                  in go newsz newbld newfin $ k bs
    go _ _ _ (Fail a b c) = Fail a b c

type GetE = ReaderT Endian G.Get

runGetE :: GetE a -> Endian -> G.Get a
runGetE = runReaderT

readEndian :: G.Get a -> G.Get a -> GetE a
readEndian le be = ReaderT $ endian le be

getWord16 :: GetE Word16
getWord16 = readEndian getWord16le getWord16be

getWord32 :: GetE Word32
getWord32 = readEndian getWord32le getWord32be

getWord64 :: GetE Word64
getWord64 = readEndian getWord64le getWord64be

getInt16 :: GetE Int16
getInt16 = readEndian getInt16le getInt16be

getInt32 :: GetE Int32
getInt32 = readEndian getInt32le getInt32be

getInt64 :: GetE Int64
getInt64 = readEndian getInt64le getInt64be

getFloat :: GetE Float
getFloat = readEndian G.getFloatle G.getFloatbe

getDouble :: GetE Double
getDouble = readEndian G.getDoublele G.getDoublebe
