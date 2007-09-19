-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2007 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Character set conversions.
--
-----------------------------------------------------------------------------
module Codec.Text.IConv (

  convert

  ) where

import Prelude hiding (length)
import Control.Exception (assert)
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as S

import qualified Codec.Text.IConv.Internal as IConv
import Codec.Text.IConv.Internal (IConv)

-- | The values permitted for input and output character set encodings and the
-- supported combinations are system dependent.
--
-- When using the GNU C or libiconv libraries, the permitted values are listed
-- by the @iconv --list@ command, and all combinations of the listed values
-- are supported.
--
type Charset = String

{-# NOINLINE convert #-}
-- | Convert the encoding of characters in input text, using the underlying
-- iconv() C library function.
--
-- The conversion is done lazily. Any charset conversion errors will result
-- in an exception. For more control over conversion errors use
-- 'convertCarefully'.
--
convert :: Charset           -- ^ Name of input character set encoding
        -> Charset           -- ^ Name of output character set encoding
        -> L.ByteString      -- ^ Input text
        -> L.ByteString      -- ^ Output text
convert fromCharset toCharset chunks =
  IConv.run fromCharset toCharset $ do
    IConv.newOutputBuffer outChunkSize
    fillInputBuffer chunks

outChunkSize :: Int
outChunkSize = L.defaultChunkSize

fillInputBuffer :: L.ByteString -> IConv L.ByteString
fillInputBuffer (L.Chunk inChunk inChunks) = do
  IConv.pushInputBuffer inChunk
  drainBuffers inChunks

fillInputBuffer L.Empty = do
  outputBufferBytesAvailable <- IConv.outputBufferBytesAvailable
  if outputBufferBytesAvailable > 0
    then do outChunk <- IConv.popOutputBuffer
            IConv.finalise
            return (L.Chunk outChunk L.Empty)
    else do IConv.finalise
            return L.Empty


drainBuffers :: L.ByteString -> IConv L.ByteString
drainBuffers inChunks = do

  inputBufferEmpty_ <- IConv.inputBufferEmpty
  outputBufferFull <- IConv.outputBufferFull
  assert (not outputBufferFull && not inputBufferEmpty_) $ return ()
  -- this invariant guarantees we can always make forward progress

  status <- IConv.iconv

  case status of
    IConv.InputEmpty -> do
      inputBufferEmpty <- IConv.inputBufferEmpty
      assert inputBufferEmpty $ fillInputBuffer inChunks

    IConv.OutputFull -> do
      outChunk <- IConv.popOutputBuffer
      outChunks <- IConv.unsafeInterleave $ do
        IConv.newOutputBuffer outChunkSize
        drainBuffers inChunks
      return (L.Chunk outChunk outChunks)

    IConv.InvalidChar -> do
      inputPos <- IConv.inputPosition
      fail ("invalid input sequence at byte offset " ++ show inputPos)

    IConv.IncompleteChar -> fixupBoundary inChunks

tmpChunkSize :: Int
tmpChunkSize = 16

fixupBoundary :: L.ByteString -> IConv L.ByteString
fixupBoundary L.Empty = do
  inputPos <- IConv.inputPosition
  fail ("incomplete input sequence at byte offset " ++ show inputPos)
fixupBoundary inChunks@(L.Chunk inChunk inChunks') = do
  inSize <- IConv.inputBufferSize
  assert (inSize < tmpChunkSize) $ return ()
  let extraBytes = tmpChunkSize - inSize

  if S.length inChunk <= extraBytes
    then do
      IConv.replaceInputBuffer (`S.append` inChunk)
      drainBuffers inChunks'
    else do
      IConv.replaceInputBuffer (`S.append` S.take extraBytes inChunk)

      before <- IConv.inputBufferSize
      assert (before == tmpChunkSize) $ return ()

      status <- IConv.iconv
      after <- IConv.inputBufferSize
      let consumed = before - after

      case status of
        IConv.InputEmpty ->
          assert (consumed == tmpChunkSize) $
          fillInputBuffer (L.Chunk (S.drop extraBytes inChunk) inChunks')

        IConv.OutputFull -> do
          outChunk <- IConv.popOutputBuffer
          outChunks <- IConv.unsafeInterleave $ do
            IConv.newOutputBuffer outChunkSize
            drainBuffers inChunks
          return (L.Chunk outChunk outChunks)

        IConv.InvalidChar -> do
          inputPos <- IConv.inputPosition
          fail ("invalid input sequence at byte offset " ++ show inputPos)


        IConv.IncompleteChar -> 
          assert (inSize < consumed && consumed < tmpChunkSize) $
          --    inSize < consumed < tmpChunkSize
          -- => { subtract inSize from each side }
          --    0 < consumed - inSize < tmpChunkSize - inSize
          -- => { by definition that extraBytes = tmpChunkSize - inSize }
          --    0 < consumed - inSize < extraBytes
          -- => { since we're in the False case of the if, we know:
          --        not (S.length inChunk <= extraBytes)
          --      =      S.length inChunk > extraBytes
          --      =      extraBytes < S.length inChunk }
          --    0 < consumed - inSize < extraBytes < S.length inChunk
          --
          -- And we're done! We know it's safe to drop (consumed - inSize) from
          -- inChunk since it's more than 0 and less than the inChunk size, so
          -- we're not being left with an empty chunk (which is not allowed).

          drainBuffers (L.Chunk (S.drop (consumed - inSize) inChunk) inChunks')
