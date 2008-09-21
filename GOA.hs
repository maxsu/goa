
{-# OPTIONS -fglasgow-exts #-}

module GOA (
    module Prelude,
    lambdabot,
    wakeup,
    query,
    setLambdabotHome,
    setLambdabotFlags
    ) where

import Data.List            (isPrefixOf, find)
import Data.Char            (isSpace)
import Data.Maybe
import System.IO
import System.Process 
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Control.Monad
import qualified Control.Exception as C 
import System.FilePath.Posix (pathSeparator)

-- |
-- Path to lambdabot directory
--
lambdabotHome :: IORef FilePath
lambdabotHome = unsafePerformIO $ do
                  userHome <- getHomeDirectory
                  home <- readFile (userHome ++ [pathSeparator] ++ ".ghci")
                  newIORef (parsedHome home)
{-# NOINLINE lambdabotHome #-}

-- Try to parse the lambdabot home, otherwise return empty string.
parsedHome :: String -> String
parsedHome = path . find (isPrefixOf prefix) . lines where
    path = trim . filter (/='"') . join . drop 1 . words . fromMaybe ""
    prefix = "setLambdabotHome"
    trim = unwords . words

lambdabotFlags :: IORef String
lambdabotFlags = unsafePerformIO $ newIORef ""
{-# NOINLINE lambdabotFlags #-}

-- | 
-- let's you customize to the location of your lambdabot install
--
setLambdabotHome :: String -> IO ()
setLambdabotHome = writeIORef lambdabotHome

-- | 
-- let's you set the lambdabot start up flags such as
--
-- @--online@ 
-- @--restricted@
--
setLambdabotFlags :: String -> IO ()
setLambdabotFlags = writeIORef lambdabotFlags

-- |
-- internal state, keep track of our in and out handles, and process id
--
data ST = ST !Handle -- ^ Handle to lambdabot stdin
             !Handle -- ^ Handle to lambdabot stdout
             !Handle -- ^ Handle to lambdabot stderr
             !ProcessHandle -- ^ lambdabot's pid

-- |
-- Module-internal state. Hang on to lambdabot's handles
--
state :: IORef (Maybe ST)
state = unsafePerformIO $ newIORef Nothing
{-# NOINLINE state #-}

-- |
-- Fork lambdabot on start up
--
wakeup :: IO ()
wakeup = do wakeup'; return ()

-- | Bool indicates success/failure
wakeup' :: IO Bool
wakeup' = do
    m <- forkLambdabot
    case m of
        Nothing        -> return False
        Just (a,b,c,d) -> do writeIORef state (Just (ST a b c d))
                             return True

-- |
-- fork a lambdabot on start up
--
-- TODO, do this hmp3-style, with a separate thread and a channel
--
-- catch error and print better message
forkLambdabot :: IO (Maybe (Handle,Handle,Handle,ProcessHandle))
forkLambdabot = withLambdabot $ do
    b <- doesFileExist "./lambdabot"
    home  <- readIORef lambdabotHome
    args' <- readIORef lambdabotFlags
    let args | null args' = []
             | otherwise  = [args']
    if not b
        then do putStrLn $ "No lambdabot binary found in: " ++ home
                return Nothing
        else C.catch
                (do x <- runInteractiveProcess "./lambdabot" args Nothing Nothing
                    return (Just x))
                (\e -> do
                    putStrLn $ "Unable to start lambdabot: " ++ show e
                    return Nothing)

-- |
-- Query lambdabot
--
lambdabot :: String -> String -> IO [Char]
lambdabot command args = withLambdabot $ do
    r <- query command args
    mapM_ putStrLn r
    return []

-- |
-- query a running lambdabot
--
-- Could try to catch a closed handle here, and run 'wakeup' again
--
query :: String -> String -> IO [String]
query command args
    | null $ command ++ args = return [] -- fixes a bug where lambdabot never
                                         -- responds, thus hanging GoA
    | otherwise = do
    m <- readIORef state
    C.handle
        (\e -> do writeIORef state Nothing -- blank old handles if we fail
                  return ["Unable to run lambdabot: " ++ show e])
        (case m of
            Nothing           -> do
              -- maybe we can start the process automatically
              success <- wakeup'
              if success then query command args else return []
            Just (ST i o _ _) -> do
                -- some commands seem to assume no whitespace at the end so we trim it
                let s = reverse . dropWhile isSpace . reverse $ unwords [command,args]
                hPutStrLn i s >> hFlush i
          --    hGetLine o -- throw away irc message (and prompt on first line)
                result <- clean `fmap` getOutput o []
                return (lines result))
        where
        clean x
              | "lambdabot> " `isPrefixOf` x = drop 11 x
              | otherwise                    = x
        -- trim was spoiling some output
        -- trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- |
-- read output until next command is seen
--
getOutput :: Handle -> String -> IO String
getOutput h acc
    | ">tobadbmal\n"          `isPrefixOf` acc = return $ reverse (drop 11 acc)
    | otherwise = do
            c <- hGetChar h
            getOutput h (c:acc)

-- |
-- perform an IO action in the lambdabot directory
--
withLambdabot :: IO a -> IO a
withLambdabot a = do

    p    <- getCurrentDirectory
    home <- readIORef lambdabotHome
    setCurrentDirectory home
    v    <- a
    setCurrentDirectory p
    return v
