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
-- | Convert the encoding of characters in input text from one named character
-- set to another.
--
-- * The conversion is done lazily.
--
-- * If conversion between the two character sets is not possible an exception
-- is thrown.
--
-- * Any charset conversion errors will result in an exception.
--
-- It uses the POSIX @iconv()@ library function. The range of available
-- character sets is determined by the capabilities of the underlying iconv
-- implementation.
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

-- | The posix iconv api looks like it's designed specifically for streaming
-- and it is, except for one really really annoying corner case...
--
-- Suppose you're converting a stream, say by reading a file in 4k chunks. This
-- would seem to be the canonical use case for iconv, reading and converting an
-- input file. However suppose the 4k read chunk happens to split a multi-byte
-- character. Then iconv will stop just before that char and tell us that its
-- an incomplete char. So far so good. Now what we'd like to do is have iconv
-- remember those last few bytes in its conversion state so we can carry on
-- with the next 4k block. Sadly it does not. It requires us to fix things up
-- so that it can carry on with the next block starting with a complete multi-
-- byte character. Do do that we have to somehow copy those few trailing bytes
-- to the beginning of the next block. That's perhaps not too bad in an
-- imperitive context using a mutable input buffer - we'd just copy the few
-- trailing bytes to the beginning of the buffer and do a short read (ie 4k-n
-- the number of trailing bytes). That's not terribly nice since it means the
-- OS has to do IO on non-page aligned buffers which tends to be slower. It's
-- worse for us though since we're not using a mutable input buffer, we're
-- using a lazy bytestring which is a sequence of immutable buffers.
--
-- So we have to do more cunning things. We could just prepend the trailing
-- bytes to the next block, but that would mean alocating and copying the whole
-- next block just to prepend a couple bytes. This probably happens quite
-- frequently so would be pretty slow. So we have to be even more cunning.
--
-- The solution is to create a very small buffer to cover the few bytes making
-- up the character spanning the block boundary. So we copy the trailing bytes
-- plus a few from the beginning of the next block. Then we run iconv again on
-- that small buffer. How many bytes from the next block to copy is a slightly
-- tricky issue. If we copy too few there's no guarantee that we have enough to
-- give a complete character. We opt for a maximum size of 16, 'tmpChunkSize'
-- on the theory that no encoding in existance uses that many bytes to encode a
-- single character, so it ought to be enough. Yeah, it's a tad dodgey.
--
-- Having papered over the block boundary, we still have to cross the boundary
-- of this small buffer. It looks like we've still got the same problem,
-- however this time we should have crossed over into bytes that are wholly
-- part of the large following block so we can abandon our small temp buffer
-- an continue with the following block, with a slight offset for the few bytes
-- taken up by the chars that fit into the small buffer.
--
-- So yeah, pretty complex. Check out the proof below of the tricky case.
--
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
