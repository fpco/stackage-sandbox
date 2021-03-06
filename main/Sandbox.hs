{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Conduit
import Control.Exception (Exception, bracket_, catch, throwIO)
import Control.Monad (when)
--import Data.Attoparsec.ByteString ((<?>))
--import qualified Data.Attoparsec.ByteString as Parse
--import qualified Data.Attoparsec.ByteString.Char8 as Parse
import Data.ByteString (ByteString)
--import Data.Conduit.Attoparsec (conduitParserEither)
--import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy as LText
import Data.Typeable (Typeable)
import Options.Applicative hiding (header, progDesc, short, action)
import Stackage.CLI
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr, openFile, IOMode(AppendMode))
--import System.IO.Error (isDoesNotExistError)
import System.Process (callProcess, readProcess)
import qualified Paths_stackage_sandbox as CabalInfo

type Snapshot = Text
type Package = Text
data Action
  = Init (Maybe Snapshot)
  | PackageDb
  | List (Maybe Package)
  | Unregister Package
  | Delete (Maybe Snapshot)
  | Upgrade (Maybe Snapshot)
  | Print PrintOpt

data PrintOpt = PrintShort | PrintFull

data SandboxException
  = NoHomeEnvironmentVariable
  | AlreadyAtSnapshot
  | PackageDbLocMismatch Text Text
  | MissingConfig Bool Bool
  | SnapshotMismatch Text Text
  | NonStackageCabalConfig
  | EmptyCabalConfig
  | DecodePathFail Text
  | ConfigAlreadyExists
  | InvalidSnapshot Snapshot
  | GhcVersionFail
  | PackageDbNotFound
  | WrongGhc Text Text Snapshot
  deriving (Show, Typeable)
instance Exception SandboxException

mSnapshotToArgs :: Maybe Snapshot -> [String]
mSnapshotToArgs = fmap T.unpack . maybeToList

version :: String
version = $(simpleVersion CabalInfo.version)

header :: String
header = "Manages shared stackage sandboxes"

progDesc :: String
progDesc = header

snapshotParser :: Parser Snapshot
snapshotParser = T.pack <$> strArgument mods where
  mods = metavar "SNAPSHOT"

packageParser :: Parser Package
packageParser = T.pack <$> strArgument mods where
  mods = metavar "PACKAGE"

packageDbDesc :: String
packageDbDesc = "Prints '--package-db $db', or prints nothing"

listDesc :: String
listDesc = "Calls `ghc-pkg list` with the sandbox package-db"

unregisterDesc :: String
unregisterDesc = "Calls `ghc-pkg unregister PACKAGE with the sandbox package-db"

deleteDesc :: String
deleteDesc = "Deletes cabal.config and cabal.sandbox.config. "
  <> "If provided with a SNAPSHOT, instead deletes that snapshot's folder."

upgradeDesc :: String
upgradeDesc = "Upgrade to the given SNAPSHOT. Defaults to the latest LTS."

printDesc :: String
printDesc = "Print the sandbox currently in use"

subcommands = do
  addCommand "init" "Init" Init (optional snapshotParser)
  addCommand "package-db" packageDbDesc (const PackageDb) (pure ())
  addCommand "list" listDesc List (optional packageParser)
  addCommand "unregister" unregisterDesc Unregister packageParser
  addCommand "delete" deleteDesc Delete (optional snapshotParser)
  addCommand "upgrade" upgradeDesc Upgrade (optional snapshotParser)
  addCommand "print" printDesc Print printOptParser

printOptParser :: Parser PrintOpt
printOptParser = flag PrintShort PrintFull $
  long "full-path"

cabalSandboxInit :: FilePath -> IO ()
cabalSandboxInit dir = do
  let dirText = T.pack dir
  let args =
        [ "sandbox"
        , "init"
        , "--sandbox"
        , T.unpack dirText
        ]
  callProcess "cabal" args

-- precondition: cabal.config exists
parseConfigSnapshot :: IO Snapshot
parseConfigSnapshot = do
  ls <- T.lines <$> T.readFile "cabal.config"
  let p = "-- Stackage snapshot from: http://www.stackage.org/snapshot/"
  let p' = "-- Stackage snapshot from: http://www.stackage.org/"
  case ls of
    (l:_) -> case T.stripPrefix p l of
      Just snapshot -> return snapshot
      Nothing -> case T.stripPrefix p' l of
        Just snapshot -> return snapshot
        Nothing -> throwIO NonStackageCabalConfig
    _ -> throwIO EmptyCabalConfig

-- TODO: a stricter check?
snapshotEq :: Snapshot -> Snapshot -> Bool
snapshotEq short full = T.isPrefixOf (T.replace "/" "-" short) full

-- TODO: verify things about the packages in the sandbox
sandboxVerify :: IO ()
sandboxVerify = do
  cabalConfigExists <- doesFileExist "cabal.config"
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  if (cabalConfigExists && cabalSandboxConfigExists)
    then do
      packageDb <- getPackageDb
      snapshot <- parseConfigSnapshot
      snapshotDir <- getSnapshotDir snapshot
      let snapshotDirText = T.pack snapshotDir
      when (not $ T.isPrefixOf snapshotDirText packageDb) $ do
        throwIO $ PackageDbLocMismatch snapshotDirText packageDb
    else do
      --throwIO $ MissingConfig cabalConfigExists cabalSandboxConfigExists
      return () -- MissingConfig is now ok.

getGhcVersion :: IO Text
getGhcVersion = do
  output <- readProcess "ghc" ["--version"] ""
  case words output of
    [] -> throwIO $ GhcVersionFail
    ws -> return $ "ghc-" <> T.pack (last ws)

sandboxInit :: Maybe Snapshot -> IO ()
sandboxInit msnapshot = do
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  when cabalSandboxConfigExists $ do
    throwIO ConfigAlreadyExists

  verifyGhcCompat msnapshot

  cabalConfigExists <- doesFileExist "cabal.config"
  configAdded <- if (not cabalConfigExists)
    then do
      runStackagePlugin "init" (mSnapshotToArgs msnapshot)
      return True
    else
      return False

  configSnapshot <- parseConfigSnapshot
  snapshot <- case msnapshot of
    Just s | snapshotEq s configSnapshot -> return configSnapshot
    Just s -> throwIO $ SnapshotMismatch s configSnapshot
    Nothing -> return configSnapshot

  -- TODO: MonadLogger
  T.putStrLn $ "Initializing at snapshot: " <> snapshot

  dir <- getSnapshotDir snapshot
  createDirectoryIfMissing True dir
  cabalSandboxInit dir
  appendFileToFile "cabal.config" "cabal.sandbox.config"
  when configAdded $ removeFile "cabal.config"
  sandboxVerify

getHome :: IO Text
getHome = T.pack <$> do
  mHome <- lookupEnv "HOME"
  case mHome of
    Just home -> return home
    Nothing -> do
      mHomePath <- lookupEnv "HOMEPATH"
      mHomeDrive <- lookupEnv "HOMEDRIVE"
      case (++) <$> mHomeDrive <*> mHomePath of
        Just home -> return home
        Nothing -> throwIO NoHomeEnvironmentVariable

getSnapshotDirPrefix :: IO FilePath
getSnapshotDirPrefix = do
  home <- getHome
  ghcVersion <- getGhcVersion
  let dir = T.unpack home </> ".stackage" </> "sandboxes"
        </> T.unpack ghcVersion
  return dir

getSnapshotDir :: Snapshot -> IO FilePath
getSnapshotDir snapshot = do
  dir <- getSnapshotDirPrefix
  return $ dir </> T.unpack snapshot


-- copied from Purge.hs, tweaked
-- TODO: remove duplication
parsePackageDb :: IO (Maybe Text)
parsePackageDb = do
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- T.readFile "cabal.sandbox.config"
      let packageDbLine = T.stripPrefix "package-db: "
      return $ listToMaybe $ mapMaybe packageDbLine $ T.lines t
    else
      return Nothing

getPackageDb :: IO Text
getPackageDb = parsePackageDb >>= \mdb -> case mdb of
  Just packageDb -> return packageDb
  Nothing -> throwIO $ PackageDbNotFound

getPackageDbArg :: IO Text
getPackageDbArg = parsePackageDb >>= \mdb -> case mdb of
  Just packageDb -> return $ "--package-db=" <> packageDb
  Nothing -> return ""


printPackageDb :: IO ()
printPackageDb = getPackageDbArg >>= T.putStrLn

ghcPkgList :: Maybe Package -> IO ()
ghcPkgList mPackage = do
  packageDb <- getPackageDbArg
  let args
        = ["list"]
       <> case mPackage of
            Nothing -> []
            Just package -> [T.unpack package]
       <> [T.unpack packageDb]
  callProcess "ghc-pkg" args

ghcPkgUnregister :: Package -> IO ()
ghcPkgUnregister package = do
  packageDb <- getPackageDbArg
  callProcess "ghc-pkg" ["unregister", T.unpack package, T.unpack packageDb]

sandboxDelete :: IO ()
sandboxDelete = do
  cabalConfigExists <- doesFileExist "cabal.config"
  when cabalConfigExists $ do
    removeFile "cabal.config"
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  when cabalSandboxConfigExists $ do
    oldSandboxNotice
    removeFile "cabal.sandbox.config"

snapshotSanityCheck :: Snapshot -> IO ()
snapshotSanityCheck snapshot =
  if any (`T.isInfixOf` snapshot) ["..", "/"]
    then throwIO $ InvalidSnapshot snapshot
    else return ()

sandboxDeleteSnapshot :: Snapshot -> IO ()
sandboxDeleteSnapshot snapshot = do
  snapshotSanityCheck snapshot
  getSnapshotDir snapshot >>= removeDirectoryRecursive

-- Find the canonical name for a snapshot by looking it up on stackage.org.
-- This can change over time. e.g. "lts" used to mean lts-1.0.
downloadSnapshot :: Maybe Snapshot -> IO Snapshot
downloadSnapshot mSnapshot = do
  currentDir <- getCurrentDirectory
  let tempDir = ".stackage-sandbox-tmp"
      enterTempDir = do
        createDirectory tempDir
        setCurrentDirectory tempDir
      exitTempDir = do
        setCurrentDirectory currentDir
        removeDirectoryRecursive tempDir
  bracket_ enterTempDir exitTempDir $ do
    runStackagePlugin "init" (mSnapshotToArgs mSnapshot)
    parseConfigSnapshot


sandboxUpgrade :: Maybe Snapshot -> IO ()
sandboxUpgrade mSnapshot = do
  cabalConfigExists <- doesFileExist "cabal.config"
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"

  mConfigSnapshot <- if cabalConfigExists
    then Just <$> parseConfigSnapshot
    else return Nothing

  snapshot <- downloadSnapshot mSnapshot

  when ((Just snapshot == mConfigSnapshot || mConfigSnapshot == Nothing)
        && cabalSandboxConfigExists) $ do
    packageDb <- getPackageDb
    snapshotDir <- getSnapshotDir snapshot
    let snapshotDirText = T.pack snapshotDir
    if T.isPrefixOf snapshotDirText packageDb
      then do
        T.putStrLn $ "Already at snapshot: " <> snapshot
        -- TODO: more verification
        throwIO AlreadyAtSnapshot
      else return ()

  sandboxDelete
  sandboxInit mSnapshot

getSandboxPrefix :: IO Text
getSandboxPrefix = do
  dirPath <- getSnapshotDirPrefix
  let dirPathText = T.pack dirPath
  let viaString f = T.pack . f . T.unpack
  return $ viaString addTrailingPathSeparator dirPathText

oldSandboxNotice :: IO ()
oldSandboxNotice = do
  db <- getPackageDb
  sandboxPrefix <- getSandboxPrefix
  case T.stripPrefix sandboxPrefix db of
    Nothing -> do
      putStrLn "Notice: Your old sandbox remains intact:"
      T.putStrLn db
    Just db' -> case T.takeWhile (not . flip elem ("/\\" :: String)) db' of
      snapshot | not (T.null snapshot) -> do
        T.putStrLn $ "Notice: The " <> snapshot <> " shared sandbox remains intact."
        T.putStrLn $ "You may delete it from your system by calling:"
        T.putStrLn $ "stackage sandbox delete " <> snapshot
      _ -> do
        putStrLn "Notice: Your old sandbox remains untouched:"
        T.putStrLn db
  T.putStrLn ""

data PackageConstraint = PackageConstraint
  { packageConstraintName :: Text
  , packageConstraintConstraint :: Constraint
  -- ^ Note(dan): I want to rename this
  }
  deriving Show

data Constraint
  = ConstraintVersionEq [Int]
  | ConstraintInstalled
  deriving Show

-- Doesn't include newline
displayConstraintLine :: PackageConstraint -> Text
displayConstraintLine c
    = "constraints: "
   <> displayPackageConstraint c

displayPackageConstraint :: PackageConstraint -> Text
displayPackageConstraint (PackageConstraint name constraint) =
  name <> " " <> case constraint of
    ConstraintVersionEq ver -> "==" <> displayVersion ver
    ConstraintInstalled -> "installed"

-- TODO: efficiency?
displayVersion :: [Int] -> Text
displayVersion
  = LText.toStrict
  . Builder.toLazyText
  . mconcat
  . List.intersperse (Builder.singleton '.')
  . map Builder.decimal

-- Copies a cabal.config into a cabal.sandbox.config
appendFileToFile :: FilePath -> FilePath -> IO ()
appendFileToFile src dest = runResourceT
   $ (sourceFile src :: Producer (ResourceT IO) ByteString)
  $$ sinkIOHandle (openFile dest AppendMode)

{-
copyConstraints :: IO ()
copyConstraints = runResourceT $
  packageConstraintsProducer $$ packageConstraintsConsumer

packageConstraintsProducer :: (MonadResource m, MonadIO m) => Producer m PackageConstraint
packageConstraintsProducer
   = sourceFile "cabal.config"
  $= linesAsciiC filterPackageConstraintLine

packageConstraintsConsumer :: (MonadResource m, MonadIO m) => Consumer PackageConstraint m ()
packageConstraintsConsumer
  --  yield "" to print an extra blank line
   = (yield "" >> mapC displayConstraintLine)
  =$ unlinesC
  =$ sinkIOHandle (openFile "cabal.sandbox.config" AppendMode)


linesAsciiC :: MonadIO m => (Int -> Conduit ByteString m b) -> Conduit ByteString m b
linesAsciiC k = do
  let getLineCount = return 0
  -- for debugging, comment the above and uncomment the below
  --lineCountRef <- liftIO $ newIORef (1 :: Int)
  --let getLineCount = liftIO $ do
  --      lineCount <- readIORef lineCountRef
  --      modifyIORef lineCountRef (+1)
  --      evaluate lineCount

  peekForever $ do
    lineCount <- getLineCount
    lineAsciiC (k lineCount)

filterPackageConstraintLine :: MonadIO m => Int -> Conduit ByteString m PackageConstraint
filterPackageConstraintLine line
    = conduitParserEither constraintLineParser
  =$= filterRightSnd
  where
    filterRightSnd = awaitForever $ \case
      Left _e -> do
        -- for debugging, uncomment the below
        -- (The first 4 lines of a stackage cabal.config are expected to fail)
        -- (while the rest are expected to succeed)
        --liftIO $ do
        --  putStrLn $ "Error parsing line: " <> show line
        --  print _e
        return ()
      Right (_, a) -> yield a

-- TODO: use or move to stackage-common
constraintLineParser :: Parse.Parser PackageConstraint
constraintLineParser
   = Parse.skipSpace
  *> optional (Parse.string "constraints:" <* Parse.skipSpace)
  *> packageConstraintParser
  <* optional Parse.skipSpace
  <* optional (Parse.char ',' <* Parse.skipSpace)
  <?> "constraintLineParser"

packageConstraintParser :: Parse.Parser PackageConstraint
packageConstraintParser
    = PackageConstraint
  <$> (packageNameParser <* Parse.skipSpace)
  <*> versionConstraintParser
  <?> "packageConstraintParser"

packageNameParser :: Parse.Parser Text
packageNameParser
   = T.pack -- TODO: efficiency?
  <$> packageNameStringParser
  <?> "packageNameParser"
  where
    packageNameStringParser
        = (:)
      <$> Parse.letter_iso8859_15
      <*> many packageNameValidCharParser


packageNameValidCharParser :: Parse.Parser Char
packageNameValidCharParser
    = Parse.letter_iso8859_15
  <|> Parse.digit
  <|> Parse.char '-'
  <?>"packageNameValidCharParser"

-- TODO: more than just ==
versionConstraintParser :: Parse.Parser Constraint
versionConstraintParser
    = (ConstraintVersionEq <$>
        (Parse.string "==" *> versionParser))
  <|> (Parse.string "installed" *> pure ConstraintInstalled)
  <?> "versionConstraintParser"

versionParser :: Parse.Parser [Int]
versionParser
     = Parse.decimal `Parse.sepBy1` Parse.char '.'
  <?> "versionParser"
-}

sandboxPrint :: PrintOpt -> IO ()
sandboxPrint printOpt = do
  packageDbText <- getPackageDb
  let path = takeDirectory  $ dropTrailingPathSeparator $ T.unpack packageDbText
  case printOpt of
    PrintFull -> putStrLn path
    PrintShort -> do
      let sandbox = takeFileName path
          preSandbox = takeDirectory path
      sandboxPrefix <- getSandboxPrefix
      if preSandbox == dropTrailingPathSeparator (T.unpack sandboxPrefix)
        then putStrLn sandbox
        else putStrLn path

-- TODO: replace with functionality from stackage-common.
approxSnapshotFor :: Maybe Snapshot -> IO Snapshot
approxSnapshotFor Nothing = return "lts"
approxSnapshotFor (Just s) = return s

-- TODO: replace with functionality from stackage-common.
approxGhcVersionFor :: Snapshot -> IO Text
approxGhcVersionFor s =
  if T.isPrefixOf "lts-2" s || T.isPrefixOf "lts-1" s || T.isPrefixOf "lts-0" s ||
     T.isPrefixOf "lts/2" s || T.isPrefixOf "lts/1" s || T.isPrefixOf "lts/0" s 
    then return "ghc-7.8.4"
    else if T.isPrefixOf "lts-3" s || T.isPrefixOf "lts/3" s ||
            s == "lts" -- assumption: current lts = 3
      then return "ghc-7.10.2"
      else
        case T.stripPrefix "nightly" s of
          Nothing -> return "ghc-7.8.4" -- Not sure what to do, default to 7.8
          Just "" -> return "ghc-7.10.2"
          Just s' | s' < "-2015-05-05" -> return "ghc-7.8.4"
          Just s' | s' < "-2015-08-06" -> return "ghc-7.10.1"
          Just _ | otherwise -> return "ghc-7.10.2"

verifyGhcCompat :: Maybe Snapshot -> IO ()
verifyGhcCompat mSnapshot = do
  ghcVersion <- getGhcVersion
  snapshot <- approxSnapshotFor mSnapshot
  ghcVersion' <- approxGhcVersionFor snapshot
  when (ghcVersion /= ghcVersion') $ do
    throwIO $ WrongGhc ghcVersion ghcVersion' snapshot


handleSandboxExceptions :: SandboxException -> IO ()
handleSandboxExceptions NoHomeEnvironmentVariable = do
  hPutStrLn stderr "Couldn't find the HOME environment variable"
  exitFailure
handleSandboxExceptions AlreadyAtSnapshot = exitSuccess
handleSandboxExceptions (PackageDbLocMismatch dir db) = do
  hPutStrLn stderr "verify: package db isn't in the expected location:"
  T.hPutStrLn stderr $ "dir: " <> dir
  T.hPutStrLn stderr $ "db: " <> db
  exitFailure
handleSandboxExceptions (MissingConfig cabalConfigExists cabalSandboxConfigExists) = do
  when (not cabalConfigExists) $
    hPutStrLn stderr "verify: cabal.config not present"
  when (not cabalSandboxConfigExists) $
    hPutStrLn stderr "verify: cabal.sandbox.config not present"
  exitFailure
handleSandboxExceptions (SnapshotMismatch s configSnapshot) = do
  T.hPutStrLn stderr
     $ "Warning: given snapshot [" <> s <> "] "
    <> "doesn't match cabal.config snapshot [" <> configSnapshot <> "]"
  hPutStrLn stderr "No action taken"
  exitFailure
handleSandboxExceptions ConfigAlreadyExists = do
  hPutStrLn stderr $ "Warning: cabal.sandbox.config already exists"
  hPutStrLn stderr $ "No action taken"
  exitFailure
handleSandboxExceptions NonStackageCabalConfig = do
  hPutStrLn stderr $ "cabal.config doesn't look like it's from stackage"
  exitFailure
handleSandboxExceptions EmptyCabalConfig = do
  hPutStrLn stderr $ "No contents found in cabal.config"
  exitFailure
handleSandboxExceptions (DecodePathFail e) = do
  hPutStrLn stderr $ "Unexpected failure decoding path:"
  T.hPutStrLn stderr e
  exitFailure
handleSandboxExceptions (InvalidSnapshot snapshot) = do
  T.hPutStrLn stderr $ "Invalid snapshot: " <> snapshot
  exitFailure
handleSandboxExceptions GhcVersionFail = do
  hPutStrLn stderr $ "Couldn't determine ghc version"
  exitFailure
handleSandboxExceptions PackageDbNotFound = do
  hPutStrLn stderr $ "Couldn't find sandbox package-db"
  exitFailure
handleSandboxExceptions (WrongGhc old new snapshot) = do
  T.hPutStrLn stderr $ "Found " <> old <> " on path"
  T.hPutStrLn stderr $ "Need  " <> new <> " to use snapshot " <> snapshot
  exitFailure

handlePluginExceptions :: PluginException -> IO ()
handlePluginExceptions (PluginNotFound _ p) = do
  hPutStrLn stderr $ "stackage-sandbox: requires plugin stackage " <> T.unpack p
  exitFailure
handlePluginExceptions (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)

main :: IO ()
main = do
  ((), action) <- simpleOptions
    version
    header
    progDesc
    (pure ())
    subcommands
  let go = case action of
        Init mSnapshot -> sandboxInit mSnapshot
        PackageDb -> printPackageDb
        List mPackage -> ghcPkgList mPackage
        Unregister package -> ghcPkgUnregister package
        Delete mSnapshot -> case mSnapshot of
          Just snapshot -> sandboxDeleteSnapshot snapshot
          Nothing -> sandboxDelete
        Upgrade mSnapshot -> sandboxUpgrade mSnapshot
        Print printOpt -> sandboxPrint printOpt
  go `catch` handleSandboxExceptions
     `catch` handlePluginExceptions
