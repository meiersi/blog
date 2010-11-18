{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      : BuilderDesign
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Explain the design of the 'Builder' monoid.
--
module BuilderDesign where

{-
Lately (here and here), I reported on promising results about the efficient construction of lazy bytestrings with a large average chunk size using the blaze-builder library. In the meantime, I have done some more research, which further confirmed the strength of the design underlying the blaze-builder library. So, before reporting on more results, I'd like to present you this design in the hope that you can apply the underlying ideas to other performance critical areas you are working on.

The Foreign module provides us with all the functions we require for working with pointers of various forms. This includes the ForeignPtr pointers used by strict bytestrings to reference their underlying buffer.
-}

import Foreign
import Data.Monoid
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

{-
A builder should represent a sequence of bytes such that the following two objectives are maximized. 

  the efficiency of writing the represented sequence of bytes to a sequence of buffers (e.g., the chunks of a lazy bytestring) and 
  the efficiency of appending two builders.

The work of a builder is handled by build steps. Once we tell a build step where it can start to write and where the buffer ends, the build step will write all the bytes it represents and return a build signal that tells us how to proceed. 
If the build signal is Done pf, then the build step has completed its work and the next free byte in the buffer is pointed to by pf. 
If the build signal is BufferFull requiredSize pf nextStep, then the build step has filled the buffer up to pf and now requires a new buffer with at least the size requiredSize. If we were creating a lazy bytestring, we would now ship of the full buffer as a chunk and allocate a new buffer that we can passt to nextStep.
The build signal InsertByteString pf bs nextStep, tells us that the build step has filled the buffer up to pf and would now like to insert a bytestring directly into the output sequence of buffers. The idea behind this signal is that it allows us to avoid copying large bytestrings.

A builder is now just a build step parametrized over the build step that should be executed after the builder has done its output.
-}

type BuildStep =  Ptr Word8     -- first free byte of buffer
	       -> Ptr Word8     -- first byte after buffer
	       -> IO BuildSignal

data BuildSignal =        -- next free byte
    Done                  !(Ptr Word8) 
  | BufferFull       !Int !(Ptr Word8)               !BuildStep
  | InsertByteString      !(Ptr Word8) !S.ByteString !BuildStep
  
newtype Builder = Builder (BuildStep -> BuildStep)

{-
Based on these definitions, we can easily define a Monoid instance that models concatenation of builders. An empty builder just returns the build step to be executed afterwards directly. We append two builders by telling the first builder that it should call the second builder once its done and by telling the second builder that it should call the continuation builder k once it is done.
-}

instance Monoid Builder where
    mempty                            = Builder id
    mappend (Builder b1) (Builder b2) = Builder $ \k -> b1 (b2 k)

{-
Now, writing an actual builder requires some care, as we are working in the IO monad and, hence, the safety belts are off. So, we simplify the implementation of serializing values that do not have to be wrapped over buffer boundaries using the notion of a Write. A value Write size io denotes a write to a buffer of size bytes, which can be executed by a call io with a pointer to the first byte that should be written. 
-}

data Write = Write Int (Ptr Word8 -> IO ())

writeWord8 :: Word8 -> Write
writeWord8 x = Write 1 (\pf -> poke pf x)

{- So defining writes is simple. Constructing a builder from a write is also not too difficult once you have understoop how to define functions such that they can call the BufferFull signal with a reference to themselves.
-}

fromWrite :: Write -> Builder
fromWrite (Write size io) =
    Builder step
  where
    step !k !pf !pe
      | pf' <= pe = do io pf
                       k pf' pe
      | otherwise = do return $ BufferFull size pf (step k)
      where
        pf' = pf `plusPtr` size
        
{- 
Once such basic builder constructors are defined, the code quickly looses it's C-like style, while retaining good performance.
-}

fromWord8 :: Word8 -> Builder
fromWord8 = fromWrite . writeWord8

fromWord8s :: [Word8] -> Builder
fromWord8s = mconcat . map fromWord8

buildABC :: Builder
buildABC = fromWord8s [65..90]

{-
The missing piece is now a driver function that actually runs a builder. A very nice property of our builder design is that it completely decouples the allocation strategy for the output buffers from the actual writing to them. Hence, whatever buffer you have ready, you can tell a builder to fill it. The only caveat is that a build step may require a large buffer than you can provide. However, all builders I implemented up to now can be wrapped at almost every point, so they would even work with a very small output buffer.

Additionally, this design also has the advantage the whole state of the driver is kept out of the performance critical funtions; i.e., the concatenation of builders and their implementation itself. This is one of the core differences to the builder implementation provided by Data.Binary.Builder from the binary package.

The driver I implement here runs a builder and executes an IO action on each full buffer denoted by a strict bytestring. We could for example use this to send the bytestring over the network using Network.Socket.ByteString. This driver is a slightly simpler to implement than the generation of a lazy bytestring. Moreover, it has the nice property that no unsafePerformIO is involved; which means that there are no other semantic pitfalls than the ones already provided by the IO monad ;-)
-}
toByteStringIOWith :: Int -> (S.ByteString -> IO ()) -> Builder -> IO ()  
toByteStringIOWith bufSize io (Builder b) = 
    fillBuffer bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    fillBuffer !size step = do
        S.mallocByteString size >>= fill
      where
        fill fpbuf = do
            let !pf = unsafeForeignPtrToPtr fpbuf
                -- safe due to later reference of fpbuf
                -- BETTER than withForeignPtr, as we lose a tail call otherwise
            signal <- step pf (pf `plusPtr` size)
            case signal of
                Done pf' -> io $ S.PS fpbuf 0 (pf' `minusPtr` pf)

                BufferFull minSize pf' nextStep  -> do
                    io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                    fillBuffer (max bufSize minSize) nextStep
                    
                InsertByteString pf' bs nextStep  -> do
                    io $ S.PS fpbuf 0 (pf' `minusPtr` pf)
                    io $ bs
                    fillBuffer bufSize nextStep
                    
{-
The implementation is rather straightforward. We construct a final step that we can hand over to the builder. Then we allocate a buffer and fill it using the given step. We handle the signals as described above and recurse using a tail call to fill the next buffer. The tail call is important to get good performance and not waste stack space. A misplaced (outermost) withForeignPtr can easily destroy this property. Therefore, I switched to reason directly about when it is safe to extract the pointer of a foreign pointer.

Lets test our driver with an artificially small chunk size.
-}

testABC :: IO ()
testABC = toByteStringIOWith 10 (putStrLn . show) buildABC

{-
*BuilderDesign> testABC                                                                                                                                                                                          
"ABCDEFGHIJ"                                                                                                                                                                                                     
"KLMNOPQRST"                                                                                                                                                                                                     
"UVWXYZ"                                                                                                                                                                                                         
-}

{- 
So this is the design of the builder of the blaze-builder library. The rest are optimizations of tight loops, judicious use of {-# UNPACK #-} pragmas and a (not yet so judicious) use of {-# INLINE #-} pragmas.

A typical optimization of a tight loop is the following example. Although the append of our builder is efficient compared to the one of the binary package, writing a list of elements can be done much more efficiently when implementing it directly as a builder using the following code.
-}

fromWriteList :: (a -> Write) -> [a] -> Builder
fromWriteList write = 
    \xs -> Builder $ step xs
  where
    step xs0 !k !pf0 !pe0 = go xs0 pf0
      where
        go []          !pf = k pf pe0
        go xs@(x':xs') !pf
          | pf' <= pe0  = io pf >> go xs' pf'
          | otherwise   = return $ BufferFull size pf (step xs k)
          where
            !pf' = pf `plusPtr` size
            Write size io = write x'

{-
One of the problems is that currently the strictness information of the arguments of a build step gets lost when calling an arbitrary build step. I assume that exposing the results from Types are Calling Conventions to the Haskell user would be a great benefit here. Moreover, I also assume that such code could be expressed more naturally in a language like DDC. I'd be very interested to see that.

Another problem with the append implementation is that it has to copy the end of buffer pointer although it does not change in most cases. One way to address this problem is to move the end of buffer pointer out of the loop as in the code above. Another option is to coalesce consecutive writes using the following Monoid instance into a single write such that the cost of an append operation can be amortized better.
-}

instance Monoid Write where
    mempty = Write 0 (const $ return ())
    mappend (Write l1 f1) (Write l2 f2) = Write (l1 + l2) $ \ptr -> do
        f1 ptr
        f2 (ptr `plusPtr` l1)


{-
I hope you did enjoy this excursion into some not so Haskell-like code. For me development of this library was (and still is) very entertaining and inspiring and I'd love to hear about how it fares in practice.
-}


