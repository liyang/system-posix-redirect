{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

{- |
Module      : System.Posix.Redirect
Copyright   : Galois, Inc. 2010
Maintainer  : ezyang@galois.com
Stability   : experimental
Portability : non-portable (POSIX, GHC)

Misbehaved third-party libraries (usually not written in Haskell)
may print error messages directly to stdout or stderr when we would
actually like to capture them and propagate them as a normal exception.
In such cases, it would be useful to temporarily override those file
descriptors to point to a pipe that we control.

This module is not portable and not thread safe.  However, it can
safely manage arbitrarily large amounts of data, as it spins off another
thread to read from the pipe created; therefore, you must use -threaded
to compile a program with this.  If you are making a foreign call,
you must ensure that the foreign call is marked safe or there is a
possibility of deadlock.

While this module is an interesting novelty, it is the module author's
opinion that it is not a sustainable method for making C libraries
behave properly, primarily due to its unportability (this trick does not
appear to be possible on Windows).  Use at your own risk.
-}

module System.Posix.Redirect
    ( redirectStdout
    , redirectStderr
    -- * Low-level operations
    , redirectWriteHandle
    , unsafeRedirectWriteFd
    ) where

import System.Posix.Types
import System.Posix.IO
import System.IO

import Foreign
import Foreign.C.Types

import Control.Concurrent
import Control.Exception

dupTo_ :: Fd -> Fd -> IO ()
dupTo_ a b = dupTo a b >>= \_ -> return ()

-- | @'unsafeRedirectFd' fd f@ executes the computation @f@, passing as
-- an argument a handle which is the read end of a pipe that
-- @fd@ now points to.  When the computation is done, the original file
-- descriptor is restored.  Use with care: if there are any file
-- handles with this descriptor that have unflushed buffers, they will
-- not flush to the old file descriptor, but the new file descriptor.
unsafeRedirectWriteFd :: Fd -> IO a -> IO (String, a)
unsafeRedirectWriteFd fd f = do
    -- setup
    (rfd, wfd) <- createPipe
    old <- dup fd
    dupTo_ wfd fd
    -- fork a thread to consume output
    outMVar <- newEmptyMVar
    outHandle <- fdToHandle rfd
    out <- hGetContents outHandle
    _ <- forkIO $ do
        _ <- evaluate (length out)
        putMVar outMVar ()
    -- run the code
    r <- f
    -- cleanup
    dupTo_ old fd
    closeFd wfd
    -- wait for output
    takeMVar outMVar
    hClose outHandle
    return (out, r)

-- | @'redirectWriteHandle' oldFd oldHandle oldCHandle f@ executes the
-- computation @f@, passing as an argument a handle which is the read
-- end of a pipe that @fd@ now points to.  This function appropriately
-- flushes the Haskell @oldHandle@ and the C @oldCHandle@ before
-- and after @f@'s execution.
redirectWriteHandle :: Fd -> Handle -> Ptr FILE -> IO a -> IO (String, a)
redirectWriteHandle oldFd oldHandle cOldHandle f = do
    hFlush oldHandle
    hFlush stdout
    _ <- c_fflush cOldHandle
    unsafeRedirectWriteFd oldFd $ do
        r <- f
        hFlush oldHandle
        _ <- c_fflush cOldHandle
        return r

-- | @'redirectStdout f' redirects standard output during the execution
-- of @f@ into a pipe passed as the first argument to @f@.
redirectStdout :: IO a -> IO (String, a)
redirectStdout f = do
    c_stdout <- cio_stdout
    redirectWriteHandle stdOutput stdout c_stdout f

-- | @'redirectStderr f' redirects standard error during the execution
-- of @f@ into a pipe passed as the first argument to @f@.
redirectStderr :: IO a -> IO (String, a)
redirectStderr f = do
    c_stderr <- cio_stderr
    redirectWriteHandle stdError stderr c_stderr f

---------------------------------------------------
-- FFI imports, since we need to flush the C buffer

data FILE

foreign import ccall safe "stdio.h fflush"
    c_fflush :: Ptr FILE -> IO CInt

foreign import ccall unsafe "hsredirect.h PosixRedirect_stdout"
    cio_stdout :: IO (Ptr FILE)
foreign import ccall unsafe "hsredirect.h PosixRedirect_stderr"
    cio_stderr :: IO (Ptr FILE)
