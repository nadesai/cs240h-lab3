module Trahs (trahs) where

import Types 
import FileActions (updateDB)
import Diff (Action(..), diff)

import Control.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.Process
import System.IO
import System.FilePath
import qualified Data.Map.Strict as S

-- | Command for executing trahs on a remote system.  The '@' will be
-- replaced by the hostname, and the directory will be appended.
trassh :: String
trassh = "ssh -CTaxq @ ./trahs --server"

-- | @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> DirectoryName -> IO ()
server r w dir = do
  hPutStrLn w "I am the server"
  line <- hGetLine r
  -- If the command asked us to switch roles, then at this point we
  -- would run client False r w dir here.  Otherwise want to process
  -- command and keep looping.

  myDir <- getCurrentDirectory
  newDB <- updateDB $ myDir </> dir
  hPutStrLn w $ show newDB
  hPutStrLn w $ "You said " ++ line
  
-- | @client turn r w dir@ runs the client to update @dir@ based on
-- the remote contents.  Commands for the remote server are written to
-- @w@, while replies are read from @r@.  If @turn@, then when done
-- the client should attempt to swap roles and run the protocol in the
-- other direction (uploading any changes to the other side).
-- Otherwise, if @turn@ is false, @client@ should simply return when
-- done.
client :: Bool -> Handle -> Handle -> DirectoryName -> IO ()
client _ r w dir = do
  line <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line
  hPutStrLn w "Hello, server"
  line' <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line'

  let remoteDB = read line' 
  myDir <- getCurrentDirectory
  localDB <- updateDB $ myDir </> dir

  let actions = S.assocs $ diff localDB remoteDB
  hPutStrLn stderr $ show actions

  let actionsIO = map (\(k,a) -> performAction r w dir k a (dbmap remoteDB)) actions
  let sequenceActions = foldl (>>=) (return $ dbmap localDB) actionsIO
  files <- sequenceActions
  hPutStrLn stderr $ show files
  
  -- At the end, if turn == True, then we issue some command to swap
  -- roles and run server r w dir.

performAction :: Handle -> Handle -> DirectoryName -> FileName -> Action -> FileStructMap -> FileStructMap -> IO FileStructMap
performAction _ _ _ _ NoChange _ files = hPutStrLn stderr "Doing nothing" >> return files
performAction _ _ dir name Delete _ files = do hPutStrLn stderr $ "Deleting " ++ name
                                               return files
                                               -- removeFile (dir </> name) 
                                               -- return $ S.delete name files 
performAction r w dir name Download _ files = do hPutStrLn stderr $ "Downloading " ++ name
                                                 return files
                                                 -- hPutStrLn w name
                                                 -- fileContents <- hGetContents r 
                                                 -- writeFile (dir </> name) fileContents
                                                 -- return files -- modify
performAction r w dir name Conflict remotefiles files = do hPutStrLn stderr $ "Resolving conflict in " ++ name
                                                           return files
                                                           {- hPutStrLn w name
                                                           fileContents <- hGetContents r
                                                           let localwstamp = wstamp $ files S.! name 
                                                           let remotewstamp = wstamp $ remotefiles S.! name
                                                           let localName = name ++ "#" ++ (show (rid localwstamp)) ++ "." ++ (show (vid localwstamp))
                                                           let remoteName = name ++ "#" ++ (show (rid remotewstamp)) ++ "." ++ (show (vid remotewstamp))
                                                           writeFile (dir </> remoteName) fileContents
                                                           renameFile (dir </> name) (dir </> localName)
                                                           return files -}

hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- maybe trassh id <$> lookupEnv "TRASSH"
  case break (== '@') tmpl of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ':dir
    _          -> return $ tmpl ++ ' ':dir

spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr $ "running " ++ show cmd
  (Just w, Just r, _, _) <- createProcess (shell cmd) {
        std_in = CreatePipe
      , std_out = CreatePipe
    }
  hSetBuffering w LineBuffering
  return (r, w)

connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir

trahs :: IO ()
trahs = do
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure
