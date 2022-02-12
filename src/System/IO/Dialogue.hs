{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module System.IO.Dialogue (Dialogue, Request (..), Bin, Name, stdin, stdout, stderr, stdecho, Response (..), IOError (..), runDialogue) where

import           Control.Concurrent   (newChan, readChan, writeChan)
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Function        ((&))
import           GHC.IO
import           Prelude              hiding (IOError)
import           System.Directory
import           System.Environment   (getArgs, getProgName, lookupEnv, setEnv)
import           System.IO            hiding (stderr, stdin, stdout)
import qualified System.IO            as Handle

type Dialogue = [Response] -> [Request]

data Request
  -- file system requests:
  = ReadFile Name
  | WriteFile Name String
  | AppendFile Name String
  | ReadBinFile Name
  | WriteBinFile Name Bin
  | AppendBinFile Name Bin
  | DeleteFile Name
  | StatusFile Name
  -- channel system requests:
  | ReadChan Name
  | AppendChan Name String
  | ReadBinChan Name
  | AppendBinChan Name Bin
  | StatusChan Name
  -- environment requests:
  | Echo Bool
  | GetArgs
  | GetProgName
  | GetEnv Name
  | SetEnv Name String
  deriving (Show, Eq, Ord)

type Bin = ByteString
type Name = String

data ChanMode = R | A deriving Eq

stdin, stdout, stderr, stdecho :: Name
stdin   = "stdin"
stdout  = "stdout"
stderr  = "stderr"
stdecho = "stdecho"

mapChan :: Name -> Maybe (Handle, ChanMode)
mapChan = \case
  "stdin"   -> Just (Handle.stdin, R)
  "stdout"  -> Just (Handle.stdout, A)
  "stderr"  -> Just (Handle.stderr, A)
  "stdecho" -> Just (Handle.stdout, A)
  _         -> Nothing

data Response
  = Success
  | Str String
  | StrList [String]
  | Bn Bin
  | Failure IOError
  deriving (Show, Eq, Ord)

data IOError
  = WriteError String
  | ReadError String
  | SearchError String
  | FormatError String
  | OtherError String
  deriving (Show, Eq, Ord)

failWith :: Applicative f => IOError -> f Response
failWith = pure . Failure

fileNotFound, chanCantRead, chanCantAppend, chanNotExist, varNotExist :: IOError
fileNotFound   = SearchError "File not found."
chanCantRead   = ReadError "Can't read from channel."
chanCantAppend = WriteError "Can't append to channel."
chanNotExist   = SearchError "Channel doesn't exist."
varNotExist    = SearchError "Environment variable doesn't exist."

ensureFileExist :: FilePath -> IO Response -> IO Response
ensureFileExist name io =
  doesFileExist name >>= \case
    False -> failWith fileNotFound
    True  -> io

withMode :: Applicative f => Name -> ChanMode -> (Handle -> f Response) -> f Response
withMode name mode f = case mapChan name of
  Nothing     -> failWith chanNotExist
  Just (h, m)
    | m == mode -> f h
    | mode == R -> failWith chanCantRead
    | otherwise -> failWith chanCantAppend

interpret :: Request -> IO Response
interpret req = case interpret' req of
    (io, err) -> catchAny io (failWith . err . show)
  where
    interpret' (ReadFile name)          = (ensureFileExist name $ Str <$> readFile name, OtherError)
    interpret' (WriteFile name str)     = (ensureFileExist name $ Success <$ writeFile name str, WriteError)
    interpret' (AppendFile name str)    = (ensureFileExist name $ Success <$ appendFile name str, WriteError)
    interpret' (ReadBinFile name)       = (ensureFileExist name $ Bn <$> LBS.readFile name, OtherError)
    interpret' (WriteBinFile name bin)  = (ensureFileExist name $ Success <$ LBS.writeFile name bin, WriteError)
    interpret' (AppendBinFile name bin) = (ensureFileExist name $ Success <$ LBS.appendFile name bin, WriteError)
    interpret' (DeleteFile name)        = (ensureFileExist name $ Success <$ removeFile name, OtherError)
    interpret' (StatusFile name)        = (, OtherError) $ do
      [p, f, d] <- mapM (unsafeInterleaveIO . ($ name)) [doesPathExist, doesFileExist, doesDirectoryExist]
      let ty = case (p, f, d) of
            (False, _, _) -> Nothing
            (_, True, _)  -> Just 'f'
            (_, _, True)  -> Just 'd'
            _             -> Just 'o'
      case ty of
        Nothing -> failWith fileNotFound
        Just c -> do
          s <- getPermissions name
          return $ Str
            [ c
            , if readable s then 'r' else '-'
            , if writable s then 'w' else '-'
            , if executable s then 'x' else '-'
            ]
    interpret' (ReadChan name)          = (withMode name R (fmap Str . hGetContents), ReadError)
    interpret' (AppendChan name str)    = (withMode name A (\h -> Success <$ (hPutStr h str >> hFlush h)), ReadError)
    interpret' (ReadBinChan name)       = (withMode name R (fmap Bn . LBS.hGetContents), ReadError)
    interpret' (AppendBinChan name str) = (withMode name A (\h -> Success <$ (LBS.hPutStr h str >> hFlush h)), ReadError)
    interpret' (StatusChan name)        = (mapChan name & maybe (failWith chanNotExist) (const (pure (Str "0 0"))), OtherError)
    interpret' (Echo b)                 = (Success <$ hSetEcho Handle.stdin b, OtherError)
    interpret' GetArgs                  = (StrList <$> getArgs, OtherError)
    interpret' GetProgName              = (Str <$> getProgName, OtherError)
    interpret' (GetEnv name)            = (lookupEnv name >>= maybe (failWith varNotExist) (pure . Str), OtherError)
    interpret' (SetEnv name str)        = (Success <$ setEnv name str, OtherError)

runDialogue :: Dialogue -> IO ()
runDialogue prog = do
  resChan <- newChan
  let reading = (:) <$> readChan resChan <*> unsafeInterleaveIO reading
  input <- unsafeInterleaveIO reading
  let output = prog input
  forM_ output $ interpret >=> writeChan resChan
