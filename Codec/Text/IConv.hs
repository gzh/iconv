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
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base as Base
import Data.ByteString.Base (LazyByteString(LPS))

import qualified Codec.Text.IConv.Internal as IConv
import Codec.Text.IConv.Internal (IConv)


{-# NOINLINE convert #-}
convert :: String -> String -> Lazy.ByteString -> Lazy.ByteString
convert fromCharset toCharset (LPS chunks) = LPS $
  IConv.run fromCharset toCharset $ do
    IConv.newOutputBuffer outChunkSize
    fillInputBuffer chunks

outChunkSize :: Int
outChunkSize = 16 * 1024 - 16

fillInputBuffer (inChunk : inChunks) = do
  IConv.pushInputBuffer inChunk
  drainBuffers inChunks

fillInputBuffer [] = do
  outputBufferBytesAvailable <- IConv.outputBufferBytesAvailable
  if outputBufferBytesAvailable > 0
    then do outChunk <- IConv.popOutputBuffer
            IConv.finalise
            return (outChunk : [])
    else do IConv.finalise
            return []


drainBuffers :: [Strict.ByteString] -> IConv [Strict.ByteString]
drainBuffers inChunks = do

  inputBufferEmpty <- IConv.inputBufferEmpty
  outputBufferFull <- IConv.outputBufferFull
  assert (not outputBufferFull && not inputBufferEmpty) $ return ()
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
      return (outChunk : outChunks)

    IConv.InvalidChar -> fail "invalid char sequence at byte ..."

    IConv.IncompleteChar -> fixupBoundary inChunks

tmpChunkSize :: Int
tmpChunkSize = 16

fixupBoundary :: [Strict.ByteString] -> IConv [Strict.ByteString]
fixupBoundary [] = fail "incomplete character sequence at byte ..."
fixupBoundary inChunks@(inChunk : inChunks') = do
  inSize <- IConv.inputBufferSize
  assert (inSize < tmpChunkSize) $ return ()
  let extraBytes = tmpChunkSize - inSize

  if Strict.length inChunk <= extraBytes
    then do
      IConv.replaceInputBuffer (`Strict.append` inChunk)
      drainBuffers inChunks'
    else do
      IConv.replaceInputBuffer (`Strict.append` Strict.take extraBytes inChunk)

      before <- IConv.inputBufferSize
      assert (before == tmpChunkSize) $ return ()

      status <- IConv.iconv
      after <- IConv.inputBufferSize
      let consumed = before - after

      case status of
        IConv.InputEmpty ->
          assert (consumed == tmpChunkSize) $
          fillInputBuffer (Strict.drop extraBytes inChunk:inChunks')

        IConv.OutputFull -> do
          outChunk <- IConv.popOutputBuffer
          outChunks <- IConv.unsafeInterleave $ do
            IConv.newOutputBuffer outChunkSize
            drainBuffers inChunks
          return (outChunk : outChunks)

        IConv.InvalidChar -> fail "invalid char sequence at byte ..."

        IConv.IncompleteChar -> 
          assert (inSize < consumed && consumed < tmpChunkSize) $
          --    inSize < consumed < tmpChunkSize
          -- => { subtract inSize from each side }
          --    0 < consumed - inSize < tmpChunkSize - inSize
          -- => { by definition that extraBytes = tmpChunkSize - inSize }
          --    0 < consumed - inSize < extraBytes
          -- => { since we're in the False case of the if, we know:
          --        not (Strict.length inChunk <= extraBytes)
          --      =      Strict.length inChunk > extraBytes
          --      =      extraBytes < Strict.length inChunk }
          --    0 < consumed - inSize < extraBytes < Strict.length inChunk
          --
          -- And we're done! We know it's safe to drop (consumed - inSize) from
          -- inChunk since it's more than 0 and less than the inChunk size, so
          -- we're not being left with an empty chunk (which is not allowed).

          drainBuffers (Strict.drop (consumed - inSize) inChunk : inChunks')
