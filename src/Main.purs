module Main (initialize) where

import Hardware.MOS6502.Emu
import Kastely.Text (toChar)
import Data.Word

import Prelude
import Data.List.Lazy ((..))
import Data.Array (range, snoc)
import Data.Integral (fromIntegral)
import Control.Monad.Reader
import Data.Maybe (Maybe(..), fromJust)
import Data.UInt (fromInt, toInt)
import Partial.Unsafe (unsafePartial)
import Data.List.Lazy (zipWithA, fromFoldable, replicateM)
import Data.Enum (upFromIncluding, enumFromTo)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Data.Foldable (for_, traverse_)
import Control.Monad.Rec.Class
import Data.String (splitAt)
import Data.String.Common (toUpper)
import Data.String.CodeUnits (fromCharArray)
-- import Data.Unfoldable (replicateA)
-- import Control.Safely (replicateM_)


import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Data.ArrayBuffer.ArrayBuffer as ArrBuf
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8, DataView, ByteOffset)
-- import Data.ArrayBuffer.ArrayBuffer as Arr
import Data.ArrayBuffer.DataView as Arr
-- import Data.ArrayBuffer.Typed as Arr

-- -- import Affjax as AX
-- -- import Affjax.Node as AN
-- -- import Affjax.RequestBody as RequestBody
-- -- import Effect.Aff (launchAff, launchAff_)
-- -- import Partial
-- -- import Affjax.ResponseFormat as ResponseFormat
-- import Node.FS.Sync
-- import Node.Buffer

newtype Memory m a = Memory (ReaderT (DataView) m a)

instance Functor m => Functor (Memory m) where
    map f = Memory <<< map f <<< runMemory
    
instance Apply m => Apply (Memory m) where
    apply ff fx = Memory $ apply (runMemory ff) (runMemory fx)

instance Applicative m => Applicative (Memory m) where
    pure = Memory <<< pure

instance Bind m => Bind (Memory m) where
    bind m k = Memory $ bind (runMemory m) (runMemory <<< k)

instance Monad m => Monad (Memory m) 

instance MonadRec m => MonadRec (Memory m) where
    tailRecM step start = Memory $ tailRecM (runMemory <<< step) start

instance MonadEffect m => MonadEffect (Memory m) where
    liftEffect act = Memory $ liftEffect act

instance MonadEffect m => MonadMachine (Memory m) where
    readMem addr = unsafePartial do
        mem <- Memory ask
        liftEffect $ fromIntegral <<< toInt <<< fromJust <$> Arr.getUint8 mem (fromIntegral addr)

    writeMem addr v = do
        mem <- Memory ask
        void $ liftEffect $ Arr.setUint8 mem (fromIntegral addr) (fromInt $ fromIntegral v)

runMemory :: forall m a. Memory m a -> ReaderT (DataView) m a
runMemory (Memory act) = act

chunksOf :: forall a. Int -> String -> Array String
chunksOf n s = tailRec go { acc: [], s: s }
  where
    go { acc: acc, s: "" } = Done acc
    go { acc: acc, s: s } = case splitAt n s of
        { before: b, after: a } -> Loop { acc: snoc acc b, s: a }

copyDataView :: DataView -> ByteOffset -> DataView -> ByteOffset -> Effect Unit
copyDataView from fromAddr to toAddr = tailRecM go 0
  where
    go i = do
        x <- Arr.getUint8 from (fromAddr + i)
        case x of
            Nothing -> pure $ Done unit
            Just x -> do
                void $ Arr.setUint8 to (toAddr + i) x
                pure $ Loop $ i + 1

readMemText :: forall m. MonadMachine m => Addr -> Addr -> m String
readMemText from len =
    map (toUpper <<< fromCharArray) <<< traverse (map toChar <<< readMem <<< fromIntegral) $
    range (fromIntegral from) (fromIntegral $ from + len - fromIntegral 1)

initialize :: (String -> Effect ArrayBuffer) -> Effect Unit
initialize loadFile = do
    mem <- Arr.whole <$> loadFile "data/program.dat"

    cpu <- new $ fromIntegral 0x438b
    let runCPU :: forall a. ReaderT CPU (Memory Effect) a -> Effect a
        runCPU = flip runReaderT mem <<< runMemory <<< flip runReaderT cpu

    let run = unsafePartial $ runCPU $ flip tailRecM unit $ \_ -> do
          getReg _.pc >>= \pc -> case fromIntegral pc of
            0x640b -> do
                let cmd = fromIntegral 0x07
                lift $ writeMem (fromIntegral 0x680d) cmd
                setReg _.regA cmd
                rts
                pure $ Loop unit

            0xcc03 -> do
                x <- getReg _.regX
                y <- getReg _.regY
                a <- getReg _.regA
                let fn = "data/disks/" <> fromCharArray (map toChar [x, y]) <> ".dat"
                liftEffect do
                    buf <- Arr.whole <$> loadFile fn
                    addr0 <- fromIntegral <<< toInt <<< fromJust <$> Arr.getUint16le buf 0
                    copyDataView buf 2 mem addr0
                rts
                pure $ Done unit

            0x40a7 -> do
                setReg _.pc $ fromIntegral 0x40bb
                pure $ Loop unit

            _ -> do
                pc <- getReg _.pc
                liftEffect $ log $ hex 4 pc
                step *> pure (Loop unit)

    -- runCPU do
    --     msg <- chunksOf 35 <$> readMemText (fromIntegral 0xfe00) (fromIntegral 510)
    --     pure $ LongMessage msg run
    run
