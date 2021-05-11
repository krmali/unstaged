module Library where

import           RIO
import           RIO.ByteString       (ByteString)
import qualified RIO.ByteString       as ByteString
import qualified RIO.ByteString.Lazy  as LazyByteString
import           System.IO            (putStrLn)
import qualified System.Process.Typed as Process

data GitStatus
  = UnstagedChanges
  | NotGitRepo
  | Clean
  | NoCommits
  | UnknownStatus ProcessOutput
  deriving (Eq , Show)

data ProcessOutput = ProcessOutput
  { standardOut   :: OutputBytes,
    standardError :: ErrorBytes
  }
  deriving (Eq, Show)

newtype OutputBytes = OutputBytes ByteString
  deriving (Eq, Show)

newtype ErrorBytes = ErrorBytes ByteString
  deriving (Eq, Show)

newtype CommandString = CommandString String deriving (Eq, Show)
newtype WorkingDirectory = WorkingDirectory FilePath  deriving (Eq, Show)

getGitStatus :: FilePath -> IO GitStatus
getGitStatus path = do
  processOutput@ProcessOutput
    { standardOut = (OutputBytes outputBytes),
      standardError = (ErrorBytes errorBytes)
    }  <-
    getProcessOutput (WorkingDirectory path) (CommandString "git status")
  -- traceShowM processOutput
  if
    | ByteString.isInfixOf "Changes not staged for commit" outputBytes -> return UnstagedChanges
    | ByteString.isInfixOf "No commits yet" outputBytes -> return NoCommits
    | ByteString.isInfixOf "nothing to commit" outputBytes -> return Clean
    | ByteString.isInfixOf "not a git repo" errorBytes -> return NotGitRepo
    | otherwise -> pure $ UnknownStatus processOutput

getProcessOutput :: WorkingDirectory -> CommandString -> IO ProcessOutput
getProcessOutput (WorkingDirectory workingDir) (CommandString commandString) = do
  case words commandString of
    command : arguments -> do
      let processConfiguration =
            Process.proc command arguments &
            Process.setStdout Process.byteStringOutput &
            Process.setStderr Process.byteStringOutput &
            Process.setWorkingDir workingDir

      Process.withProcessWait processConfiguration $ \process -> atomically $ do
        outBytes <- Process.getStdout process
        errorBytes <- Process.getStderr process
        let standardo = OutputBytes $ LazyByteString.toStrict  outBytes
            standarde = ErrorBytes $ LazyByteString.toStrict  errorBytes
        pure $ ProcessOutput standardo standarde
    [] -> error "Empty command string"

runMain :: IO ()
runMain = do
  putStrLn "Hello, World!"
