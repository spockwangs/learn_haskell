{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module RunProcess where

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Exit
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Control.Exception

{- | The type for running external commands.  The first part
of the tuple is the program name.  The list represents the
command-line parameters to pass to the command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String,              -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus    -- ^ IO action that yields exit result
    }

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command.  Returns a String
         representing the output of the command. -}
    invoke :: a -> [Fd] -> Fd -> Fd -> IO ProcessID

-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds stdinFd stdoutFd =
        do -- Create two pipes: one to handle stdin and the other
           -- to handle stdout.  We do not redirect stderr in this program.

          -- We add the parent FDs to this list because we always need
          -- to close them in the clients.

          -- Now, grab the closed FDs list and fork the child.
          childPID <- forkProcess (child closefds stdinFd stdoutFd)

          -- Now, on the parent, close the client-side FDs.
          closeFd stdinFd
          closeFd stdoutFd
          return childPID

      -- Define what happens in the child proces
      where child closefds stdinread stdoutwrite =
              do -- Copy our pipes over the regular stdin/stdout FDs
                dupTo stdinread stdInput
                dupTo stdoutwrite stdOutput

                -- Now close the original pipe FDs
                closeFd stdinread
                closeFd stdoutwrite

                -- Close all the open FDs we inherited from the parent
                mapM_ (\fd -> catch (closeFd fd) (\e -> let _ = e :: SomeException in return ())) closefds

                -- Start the program
                executeFile cmd True args Nothing

{- | Type representing a pipe.  A 'PipeCommand' consists of a source
and destination part, both of which must be instances of
'CommandLike'. -}
data (CommandLike src, CommandLike dest) =>
     PipeCommand src dest = PipeCommand src dest

{- | A convenient function for creating a 'PipeCommand'. -}
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b
(-|-) = PipeCommand

{- | Make 'PipeCommand' runnable as a command -}
instance (CommandLike a, CommandLike b) =>
         CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds stdinFd stdoutFd =
        do (readEnd, writeEnd) <- createPipe
           _ <- invoke src (readEnd:closefds) stdinFd writeEnd
           invoke dest (writeEnd:closefds) readEnd stdoutFd

{- | Execute a 'CommandLike'. -}
runIO :: CommandLike a => a -> IO ()
runIO cmd = do -- Initialize our closefds list
  (stdinRead, stdinWrite) <- createPipe
  (stdoutRead, stdoutWrite) <- createPipe

  let closeFds = [stdinWrite, stdoutRead]

  -- Invoke the command
  childPid <- invoke cmd closeFds stdinRead stdoutWrite

  -- Write something to the stdin of the first command.
  stdinWriteHandle <- fdToHandle stdinWrite
  hPutStr stdinWriteHandle []
  closeFd stdinWrite

  -- Process its output
  stdoutReadHandle <- fdToHandle stdoutRead
  output <- hGetContents stdoutReadHandle
  putStr output

  -- Wait for termination and get exit status
  status <- getProcessStatus True False childPid
  case status of
    Nothing -> fail "Error"
    Just (Exited ExitSuccess) -> return ()
    Just x -> fail $ "Exited: " ++ show x
