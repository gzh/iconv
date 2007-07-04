-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2007 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- IConv wrapper layer
--
-----------------------------------------------------------------------------
module Codec.Text.IConv.Internal (

  -- * The iconv state monad
  IConv,
  run,
  unsafeInterleave,
  unsafeLiftIO,
  finalise,

  -- * The buisness
  iconv,
  Status(..),

  -- * Buffer management
  -- ** Input buffer
  pushInputBuffer,
  inputBufferSize,
  inputBufferEmpty,
  replaceInputBuffer,

  -- ** Output buffer
  newOutputBuffer,
  popOutputBuffer,
  outputBufferBytesAvailable,
  outputBufferFull,

  -- * Debugging
--  consistencyCheck,
  dump,
  trace
  ) where

import Foreign
import Foreign.C
import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (hPutStrLn, stderr)
import Control.Exception (assert)

import Prelude hiding (length)


pushInputBuffer :: B.ByteString -> IConv ()
pushInputBuffer (B.PS inBuffer' inOffset' inLength') = do

  -- must not push a new input buffer if the last one is not used up
  inAvail <- gets inLength
  assert (inAvail == 0) $ return ()

  -- now set the available input buffer ptr and length
  modify $ \bufs -> bufs { 
    inBuffer = inBuffer',
    inOffset = inOffset',
    inLength = inLength'
  }


inputBufferEmpty :: IConv Bool
inputBufferEmpty = gets ((==0) . inLength)


inputBufferSize :: IConv Int
inputBufferSize = gets inLength


replaceInputBuffer :: (B.ByteString -> B.ByteString) -> IConv ()
replaceInputBuffer replace = do
  modify $ \bufs@Buffers {
    inBuffer = inBuffer,
    inOffset = inOffset,
    inLength = inLength
  } -> case replace (B.PS inBuffer inOffset inLength) of
         B.PS inBuffer' inOffset' inLength' ->
           bufs {
             inBuffer = inBuffer',
             inOffset = inOffset',
             inLength = inLength'
           }


newOutputBuffer :: Int -> IConv ()
newOutputBuffer size = do

  --must not push a new buffer if there is still data in the old one
  outAvail <- gets outLength
  assert (outAvail == 0) $ return ()
  -- Note that there may still be free space in the output buffer, that's ok,
  -- you might not want to bother completely filling the output buffer say if
  -- there's only a few free bytes left.

  -- now set the available output buffer ptr and length
  outBuffer <- unsafeLiftIO $ B.mallocByteString size
  modify $ \bufs -> bufs {
      outBuffer = outBuffer,
      outOffset = 0,
      outLength = 0,
      outFree   = size
    }


-- get that part of the output buffer that is currently full
-- (might be 0, use outputBufferBytesAvailable to check)
-- this may leave some space remaining in the buffer
popOutputBuffer :: IConv B.ByteString
popOutputBuffer = do

  Buffers {
    outBuffer = outBuffer,
    outOffset = outOffset,
    outLength = outLength
    } <- get

  -- there really should be something to pop, otherwise it's silly
  assert (outLength > 0) $ return ()

  modify $ \buf -> buf {
      outOffset = outOffset + outLength,
      outLength = 0
    }

  return (B.PS outBuffer outOffset outLength)


-- this is the number of bytes available in the output buffer
outputBufferBytesAvailable :: IConv Int
outputBufferBytesAvailable = gets outLength


-- you only need to supply a new buffer when there is no more output buffer
-- space remaining
outputBufferFull :: IConv Bool
outputBufferFull = gets ((==0) . outFree)


----------------------------
-- IConv buffer layout
--

data Buffers = Buffers {
    inBuffer  :: {-# UNPACK #-} !(ForeignPtr Word8), -- ^ Current input buffer
    inOffset  :: {-# UNPACK #-} !Int,                -- ^ Current read offset
    inLength  :: {-# UNPACK #-} !Int,                -- ^ Input bytes left
    outBuffer :: {-# UNPACK #-} !(ForeignPtr Word8), -- ^ Current output buffer
    outOffset :: {-# UNPACK #-} !Int,                -- ^ Base out offset
    outLength :: {-# UNPACK #-} !Int,                -- ^ Available output bytes
    outFree   :: {-# UNPACK #-} !Int                 -- ^ Free output space
  } deriving Show

nullBuffers :: Buffers
nullBuffers = Buffers B.nullForeignPtr 0 0 B.nullForeignPtr 0 0 0

-- For the output buffer we have this setup:
--
-- +-------------+-------------+----------+
-- |### poped ###|** current **|   free   |
-- +-------------+-------------+----------+
--  \           / \           / \         /
--    outOffset     outLength     outFree
--
-- The output buffer is allocated by us and pointer to by the outBuf ForeignPtr.
-- An initial prefix of the buffer that we have already poped/yielded. This bit
-- is immutable, it's already been handed out to the caller, we cannot touch it.
-- When we yield we increment the outOffset. The next part of the buffer between
-- outBuf + outOffset and outBuf + outOffset + outLength is the current bit that
-- has had output data written into it but we have not yet yielded it to the
-- caller. Finally, we have the free part of the buffer. This is the bit we
-- provide to iconv to be filled. When it is written to, we increase the
-- outLength and decrease the outLeft by the number of bytes written.

-- The input buffer layout is much simpler, it's basically just a bytestring:
--
-- +------------+------------+
-- |### done ###|  remaining |
-- +------------+------------+
--  \          / \          /
--    inOffset     inLength
--
-- So when we iconv we increase the inOffset and decrease the inLength by the
-- number of bytes read.


----------------------------
-- IConv monad
--

newtype IConv a = I {
    unI :: ConversionDescriptor
        -> Buffers
        -> IO (Buffers, a)
  }

instance Monad IConv where
  (>>=)  = bindI
--  m >>= f = (m `bindI` \a -> consistencyCheck `thenI` returnI a) `bindI` f
  (>>)   = thenI
  return = returnI
  fail   = (finalise >>) . failI

returnI :: a -> IConv a
returnI a = I $ \_ bufs -> return (bufs, a)
{-# INLINE returnI #-}

bindI :: IConv a -> (a -> IConv b) -> IConv b
bindI m f = I $ \cd bufs -> do
  (bufs', a) <- unI m cd bufs
  unI (f a) cd bufs'
{-# INLINE bindI #-}

thenI :: IConv a -> IConv b -> IConv b
thenI m f = I $ \cd bufs -> do
  (bufs', _) <- unI m cd bufs
  unI f cd bufs'
{-# INLINE thenI #-}

failI :: String -> IConv a
failI msg = I $ \_ _ -> fail (moduleErrorPrefix ++ msg)

moduleErrorPrefix :: String
moduleErrorPrefix = "Codec.Text.IConv: "
{-# NOINLINE moduleErrorPrefix #-}

{-# NOINLINE run #-}
run :: String -> String -> IConv a -> a
run from to m = unsafePerformIO $ do
  ptr <- withCString from $ \fromPtr ->
         withCString to $ \toPtr ->
           c_iconv_open toPtr fromPtr -- note arg reversal

  if ptrToIntPtr ptr == (-1)
    then do errno <- getErrno
            if errno == eINVAL
              then fail $ "cannot convert from character set "
                       ++ show from ++ " to character set " ++ show to
              else throwErrno (moduleErrorPrefix ++ "unexpected iconv error")
    else do cd <- newForeignPtr c_iconv_close ptr
            (_,a) <- unI m (ConversionDescriptor cd) nullBuffers
            return a

unsafeLiftIO :: IO a -> IConv a
unsafeLiftIO m = I $ \_ bufs -> do
  a <- m
  return (bufs, a)

-- It's unsafe because we discard the values here, so if you mutate anything
-- between running this and forcing the result then you'll get an inconsistent
-- iconv state.
unsafeInterleave :: IConv a -> IConv a
unsafeInterleave m = I $ \iconv st -> do
  res <- unsafeInterleaveIO (unI m iconv st)
  return (st, snd res)

get :: IConv Buffers
get = I $ \_ buf -> return (buf, buf)

gets :: (Buffers -> a) -> IConv a
gets getter = I $ \_ buf -> return (buf, getter buf)

modify :: (Buffers -> Buffers) -> IConv ()
modify change = I $ \_ buf -> return (change buf, ())

----------------------------
-- Debug stuff
--

trace :: String -> IConv ()
trace = unsafeLiftIO . hPutStrLn stderr


dump :: IConv ()
dump = do
  bufs <- get
  unsafeLiftIO $ hPutStrLn stderr $ show bufs

----------------------------
-- iconv wrapper layer
--

data Status =
         InputEmpty
       | OutputFull
       | IncompleteChar
       | InvalidChar
  deriving Show

iconv :: IConv Status
iconv = I $ \(ConversionDescriptor cdfptr) bufs@Buffers {
    inBuffer  = inBuffer,
    inOffset  = inOffset,
    inLength  = inLength,
    outBuffer = outBuffer,
    outOffset = outOffset,
    outLength = outLength,
    outFree   = outFree
  } ->
  assert (outFree > 0) $
  withForeignPtr cdfptr                              $ \cdPtr -> 
  withForeignPtr inBuffer                            $ \inBufPtr ->
  with (inBufPtr `plusPtr` inOffset)                 $ \inBufPtrPtr ->
  with (fromIntegral inLength)                       $ \inLengthPtr ->
  withForeignPtr outBuffer                           $ \outBufPtr ->
  with (outBufPtr `plusPtr` (outOffset + outLength)) $ \outBufPtrPtr ->
  with (fromIntegral outFree)                        $ \outFreePtr -> do
    
    result <- c_iconv cdPtr inBufPtrPtr inLengthPtr outBufPtrPtr outFreePtr
    inLength' <- fromIntegral `fmap` peek inLengthPtr
    outFree'  <- fromIntegral `fmap` peek outFreePtr
    let bufs' = bufs {
            inOffset  = inOffset + (inLength - inLength'),
            inLength  = inLength',
            outLength = outLength + (outFree - outFree'),
            outFree   = outFree'
          }
    if result /= errVal
      then return (bufs', InputEmpty)
      else do errno <- getErrno
              case () of
                _ | errno == e2BIG  -> return (bufs', OutputFull)
                  | errno == eINVAL -> return (bufs', IncompleteChar)
                  | errno == eILSEQ -> return (bufs', InvalidChar)
                  | otherwise       -> throwErrno $ moduleErrorPrefix
                                                 ++ "unexpected iconv error"
      
    
      
  where errVal :: CSize
        errVal = (-1)   -- (size_t)(-1)

-- | This never needs to be used as the iconv descriptor will be released
-- automatically when no longer needed, however this can be used to release
-- it early. Only use this when you can guarantee that the iconv will no
-- longer be needed, for example if an error occurs or if the input stream
-- ends.
--
finalise :: IConv ()
finalise = I $ \(ConversionDescriptor cd) bufs -> do
  finalizeForeignPtr cd
  return (bufs, ())


----------------------
-- The foreign imports

newtype ConversionDescriptor = ConversionDescriptor (ForeignPtr ConversionDescriptor) -- iconv_t

foreign import ccall unsafe "iconv.h iconv_open"
  c_iconv_open :: CString  -- to code
               -> CString  -- from code
               -> IO (Ptr ConversionDescriptor)

foreign import ccall unsafe "iconv.h iconv"
  c_iconv :: Ptr ConversionDescriptor
          -> Ptr (Ptr CChar)  -- in buf
          -> Ptr CSize        -- in buf bytes left
          -> Ptr (Ptr CChar)  -- out buf
          -> Ptr CSize        -- out buf bytes left
          -> IO CSize

foreign import ccall unsafe "iconv.h &iconv_close"
  c_iconv_close :: FinalizerPtr ConversionDescriptor
