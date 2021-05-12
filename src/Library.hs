module Library where

import           RIO
import qualified RIO.ByteString       as ByteString
import qualified RIO.ByteString.Lazy  as LazyByteString
import qualified RIO.Directory        as Directory
import qualified RIO.List             as List
import qualified System.Environment   as Environment
import           System.IO            (print, putStrLn)
import qualified System.Process.Typed as Process

data GitStatus
  = UnstagedChanges FilePath
  | NotGitRepo
  | Clean FilePath
  | NoCommits FilePath
  | UnknownStatus FilePath ProcessOutput
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

getSubDirectories :: FilePath  -> IO [FilePath]
getSubDirectories path = do
  files <- Directory.listDirectory path
  let filesWithCompletePath = fmap ((List.dropWhileEnd (== '/') path ++ "/") ++) files
  filterM Directory.doesDirectoryExist filesWithCompletePath

getGitStatuses :: FilePath  -> IO [GitStatus]
getGitStatuses path = do
  localGitStatus <- getGitStatus path
  case localGitStatus of
    NotGitRepo -> do
      subDirectories <- getSubDirectories path
      concat <$> traverse getGitStatuses subDirectories
      -- foldMap getGitStatuses subDirectories
    _ -> pure $ pure localGitStatus

getGitStatus :: FilePath -> IO GitStatus
getGitStatus path = do
  processOutput@ProcessOutput
    { standardOut = (OutputBytes outputBytes),
      standardError = (ErrorBytes errorBytes)
    }  <-
    getProcessOutput (WorkingDirectory path) (CommandString "git status")
  -- traceShowM processOutput
  if
    | ByteString.isInfixOf "Changes not staged for commit" outputBytes -> return $ UnstagedChanges path
    | ByteString.isInfixOf "No commits yet" outputBytes -> return $ NoCommits path
    | ByteString.isInfixOf "nothing to commit" outputBytes -> return $ Clean path
    | ByteString.isInfixOf "not a git repo" errorBytes -> return NotGitRepo
    | otherwise -> pure $ UnknownStatus path processOutput

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
  args <- Environment.getArgs
  case args of
    [path] -> do
      statuses <- getGitStatuses path
      let unstaged = foldr (\status paths -> case status of
            UnstagedChanges p -> p : paths
            _                 -> paths
            )
            [] statuses
            & List.sort
      forM_ unstaged putStrLn
    _ -> error "path to search should be first argument"
