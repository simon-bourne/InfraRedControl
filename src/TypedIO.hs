module TypedIO (
    StdoutWriter(..),
    Consumer(..),
    Producer(..),
    Controller(..),
    Sleeper(..),
    RunProcess(..),
    CurrentTime(..),
    FileReader(..),
    FileWriter(..),
    DirectoryReader(..)
) where

import qualified Control.Concurrent as Concurrent
import Control.Concurrent (Chan, ThreadId)
import qualified Data.Time.Clock as Clock
import Data.Time.Clock (UTCTime)
import qualified System.Directory as Directory
import qualified System.Process as Process
import System.Exit
import System.IO (Handle, IOMode(..), withFile)

class Monad m => StdoutWriter m where
    writeStdout :: String -> m ()
    writeStdoutLn :: String -> m ()

instance StdoutWriter IO where
    writeStdout = putStr
    writeStdoutLn = putStrLn

class Monad m => Consumer m where
    readChan :: Chan a -> m a

instance Consumer IO where
    readChan = Concurrent.readChan

class Monad m => Producer m where
    writeChan :: Chan a -> a -> m ()

instance Producer IO where
    writeChan = Concurrent.writeChan

class (Producer m, Consumer m) => Controller m where
    newChan :: m (Chan a)
    fork :: m () -> m ThreadId

instance Controller IO where
    newChan = Concurrent.newChan
    fork = Concurrent.forkIO

class Monad m => Sleeper m where
    sleep :: Int -> m ()

instance Sleeper IO where
    sleep = Concurrent.threadDelay

class Monad m => RunProcess m where
    readProcessWithExitCode :: String -> [String] -> String -> m (ExitCode, String, String)
    runProcessWithExitCode :: String -> [String] -> m ExitCode
    runProcessOrFail :: String -> [String] -> m ()

instance RunProcess IO where
    readProcessWithExitCode = Process.readProcessWithExitCode
    runProcessWithExitCode command args = do
        (exitCode, _, _) <- Process.readProcessWithExitCode command args ""
        return exitCode
    runProcessOrFail = Process.callProcess

class Monad m => CurrentTime m where
    getCurrentTime :: m UTCTime

instance CurrentTime IO where
    getCurrentTime = Clock.getCurrentTime

class Monad m => FileReader m where
    readFromFile :: FilePath -> m String
    withReadOnlyFile :: FilePath -> (Handle -> m a) -> m a

instance FileReader IO where
    readFromFile = readFile
    withReadOnlyFile path = withFile path ReadMode

class Monad m => FileWriter m where
    writeToFile :: FilePath -> String -> m ()

instance FileWriter IO where
    writeToFile = writeFile

class Monad m => DirectoryReader m where
    doesFileExist :: FilePath -> m Bool
    getDirectoryContents :: FilePath -> m [FilePath]

instance DirectoryReader IO where
    doesFileExist = Directory.doesFileExist
    getDirectoryContents = Directory.getDirectoryContents
