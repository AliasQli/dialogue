{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : System.IO.Dialogue 
Description : Stream-based I/O.
Copyright   : (c) Alias Qli, 2022
License     : BSD-3-Clause
Maintainer  : 2576814881@qq.com
Stability   : experimental
Portability : POSIX

This module implements stream-based I/O as described in Haskell Report 1.2. 

As th resport says, "Haskell's I/O system is based on the view that a program communicates to
the outside world via /streams of messages/: a program issues a stream of /requests/ to the 
operating system and in  return receives a stream of /responses/." And a stream in Haskell is 
only a lazy list.
-}

module System.IO.Dialogue 
  ( -- * The Program Type
    Dialogue
  , -- ** Request Types
    Request (..)
  ,  -- *** The Binary Type
    Bin
  , nullBin
  , appendBin
  , isNullBin
  , -- *** The 'Name' Type
    Name
  , -- **** Channels
    stdin
  , stdout
  , stderr
  , stdecho
  , -- ** Response Types
    Response (..)
  , IOError (..)
  , -- * Run the Program
    runDialogue
  ) where

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

-- * The Program Type

-- | Type of a Haskell program.
-- @['Response']@ is an ordered list of /responses/ and @['Request']@ is an ordered list of /requests/;
-- the /n/th response is the operating system's reply to the /n/th request.
type Dialogue = [Response] -> [Request]

-- | /Requests/ a program may issue.
data Request
  -- file system requests:
  = -- | @'ReadFile' name@ returns the contents of file @name@.
    --
    --   * If successful, the response will be of the form @'Str' s@, where @s@ is a string value;
    --
    --   * If the file does not exist, the response @'Failure' ('SearchError' string)@ is induced;
    --
    --   * If it is unreachable for some other reason, the @'Failure' ('ReadError' string)@ error is induced.
    ReadFile Name
  | -- | @'WriteFile' name string@ writes @string@ to file @name@. If the file does not exist, it is
    -- created. If it already exists, it is overwritten.
    --
    --   * A successful response has form @'Success'@;
    --
    --   * The only failure possible has the form @'Failure' ('WriteError' string)@.
    --
    -- This request is "hyperstrict" in its second argument: no response is
    -- returned until the entire list of values is completely evaluated.
    WriteFile Name String
  | -- | Identicle to 'WriteFile', except that
    --
    --   (1) the @string@ argument is appended to the current contents of the file named @name@;
    --
    --   (2) If the file does not exist, the response @'Failure' ('SearchError' string)@ is induced.
    --
    -- All other errors have form @'Failure' ('WriteError' string)@,
    -- and the request is hyperstrict in its second argument.
    AppendFile Name String
  | -- | Similar to 'ReadFile', except that if successful, the response will be of the form @'Bn' b@,
    -- where @b@ is a binary value.
    ReadBinFile Name
  | -- | Similar to 'WriteFile', except that it writes a binary value to file.
    WriteBinFile Name Bin
  | -- | Similar to 'AppendFile', except that it writes a binary value to file.
    AppendBinFile Name Bin
  | -- | @'DeleteFile' name@ delete file @name@, with successful response @'Success'@.
    --
    --   * If the file does not exist, the response @'Failure' ('SearchError' string)@ is induced.
    --
    --   * If it cannot be deleted for some other reason,
    --     a response of the form @'Failure' ('WriteError' string)@ is induced.
    DeleteFile Name
  | -- | @'StatusFile' name@ induce @'Failure' ('SearchError' string)@ if an object @name@ does not exist,
    -- or @'Failure' ('OtherError' string)@ if any other error should occur. Otherwise induce
    -- @'Str' status@ where @ststus@ is a string containing, in this order:
    --
    --   (1) Either \'f', \'d', or \'u' depending on whether the object is a file, directory,
    --       or something else, respectively;
    --
    --   (2) \'r' if the object is readable by this program, \'-' if not;
    --
    --   (3) \'w' if the object is writable by this program, \'-' if not;
    --
    --   (4) \'x' if the object is executable by this program, \'-' if not.
    --
    -- For example, "dr--" denotes a directory that can be read but not written or executed.
    StatusFile Name
  -- channel system requests:
  | -- | @'ReadChan' name@ opens channel @name@ for input.
    --
    --   * A successful response returns the contents of the channel as a lazy stream of characters.
    --
    --   * If the channel does not exist, the response @'Failure' ('SearchError' string)@ is induced;
    --
    --   * All other errors have form @'Failure' ('ReadError' string)@.
    --
    -- Unlike files, once a 'ReadChan' or 'ReadBinChan' request has been issued for a particular channel,
    -- it cannot be issued again for the same channel in that program, This reflects the ephemeral nature
    -- of its contents and prevents a serious space leak.
    --
    -- /Known issue/: This request would leave the handle behind the channel in /semi-closed/ state,
    -- causing any other attempt to read from the channel to fail. This should be problematic if your program
    -- issued an request to read from @stdin@, and
    --
    --   (1) You called Haskell functions that read from @stdin@ (/e.g./ 'getLine'), or ran another program
    --       that issues such a request after the program finishes.
    --
    --   (2) You're running the program from @ghci@.
    ReadChan Name
  | -- | @'AppendChan' name string@ writes @string@ to channel @name@. The sematics is as for 'AppendFile', except:
    --
    --   (1) The second argument is appended to whatever was previously written (if anything);
    --
    --   (2) If channel does not exist, the response @'Failure' ('SearchError' string)@ is induced.
    --
    -- All other errors have form @'Failure' ('WriteError' string)@.
    -- This request is hyperstrict in its second argument.
    AppendChan Name String
  | -- | Similar to 'ReadChan', except that if successful, the response will be of the form @'Bn' b@,
    -- where @b@ is a lazy binary value.
    ReadBinChan Name
  | -- | Similar to 'AppendChan', except that it writes a binary value to the channel.
    AppendBinChan Name Bin
  | -- | @'StatusChan' name@ induces @'Failure' ('SearchError' string)@ if an channel @name@ does not exist,
    -- otherwise it always induces @'Str' "0 0"@. The two @"0"@s indicate that there's no bound on the
    -- maximum line length and page length allowed on the channel, respectively.
    StatusChan Name
  -- environment requests:
  | -- | @'Echo' 'True'@ enables echoing of @stdin@ on @stdecho@; @'Echo' 'False'@ disables it. Either
    -- @'Success'@ or @'Failure' ('OtherError' string)@ is induced.
    --
    -- The report requires that the echo mode can only be set once by a particular program, before any
    -- I/O operation involving @stdin@. However, the restriction is loosened, and echo mode may be set
    -- at any time by the proogram multiple times.
    --
    -- /Known issue/: It's currently implemented as 'hSetEcho', which is known not to work on Windows.
    Echo Bool
  | -- | Induces the response @'StrList' str_list@, where @str_list@ is a list of the program's explicit
    -- command line arguments.
    GetArgs
  | -- | Returns the short name of the current program, not including search path information. If successful,
    -- the response will be of the form @'Str' s@, where @s@ is a string. If the operating system is unable
    -- to provbide the program name, @'Failure' ('OtherError' string)@ is induced.
    GetProgName
  | -- | @'GetEnv' name@ Returns the value of environment variable @name@. If successful, the response will be
    -- of the form @'Str' s@, where @s@ is a string. If the environment variable does not exist,
    -- a 'SearchError' is induced.
    GetEnv Name
  | -- | @'SetEnv' name string@ sets environment variable @name@ to @string@, with response @'Success'@.
    -- If the environment variable does not exist, it is created.
    SetEnv Name String
  deriving (Read, Show, Eq, Ord)

-- | 'Bin' is a datatype for binary values, as required by the report, and is implemented as a lazy 'ByteString'.
type Bin = ByteString

-- | The empty binary value.
nullBin :: Bin
nullBin = LBS.empty

-- | Append two 'Bin's.
appendBin :: Bin -> Bin -> Bin
appendBin = LBS.append

-- | Test whether a 'Bin' is empty.
isNullBin :: Bin -> Bool
isNullBin = LBS.null

-- | This type synonym is described in Haskell Report 1.0, and exists for backward compatibility.
type Name = String

-- | The @stdin@ channel. Readable.
stdin :: Name
stdin = "stdin"

-- | The @stdout@ channel. Writable.
stdout :: Name
stdout = "stdout"

-- | The @stderr@ channel. Writable.
stderr :: Name
stderr = "stderr"

-- | The @stdecho@ channel. Writable. Attached to @stdout@.
stdecho :: Name
stdecho = "stdecho"

data ChanMode = R | A deriving Eq

mapChan :: Name -> Maybe (Handle, ChanMode)
mapChan = \case
  "stdin"   -> Just (Handle.stdin, R)
  "stdout"  -> Just (Handle.stdout, A)
  "stderr"  -> Just (Handle.stderr, A)
  "stdecho" -> Just (Handle.stdout, A)
  _         -> Nothing

-- | /Responses/ a program may receive.
data Response
  = Success
  | Str String
  | StrList [String]
  | Bn Bin
  | Failure IOError
  deriving (Read, Show, Eq, Ord)

data IOError
  = WriteError String
  | ReadError String
  | SearchError String
  | -- | Since we're using a modern device and the maximum line length and page length 
    -- allowed on the channel have no bound, this error would never occur.
    FormatError String
  | OtherError String
  deriving (Read, Show, Eq, Ord)

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
    interpret' (ReadFile name)          = (ensureFileExist name $ Str <$> readFile name, ReadError)
    interpret' (WriteFile name str)     = (Success <$ writeFile name str, WriteError)
    interpret' (AppendFile name str)    = (ensureFileExist name $ Success <$ appendFile name str, WriteError)
    interpret' (ReadBinFile name)       = (ensureFileExist name $ Bn <$> LBS.readFile name, ReadError)
    interpret' (WriteBinFile name bin)  = (Success <$ LBS.writeFile name bin, WriteError)
    interpret' (AppendBinFile name bin) = (ensureFileExist name $ Success <$ LBS.appendFile name bin, WriteError)
    interpret' (DeleteFile name)        = (ensureFileExist name $ Success <$ removeFile name, WriteError)
    interpret' (StatusFile name)        = (, OtherError) $ do
      [p, f, d] <- mapM (unsafeInterleaveIO . ($ name)) [doesPathExist, doesFileExist, doesDirectoryExist]
      let ty = case (p, f, d) of
            (False, _, _) -> Nothing
            (_, True, _)  -> Just 'f'
            (_, _, True)  -> Just 'd'
            _             -> Just 'u'
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

-- | The central function to run a program.
runDialogue :: Dialogue -> IO ()
runDialogue prog = do
  resChan <- newChan
  let reading = unsafeInterleaveIO $ (:) <$> readChan resChan <*> reading
  input <- reading
  let output = prog input
  forM_ output $ interpret >=> writeChan resChan
