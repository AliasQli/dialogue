{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module      : System.IO.Continuation
Description : Continuation-based I/O.
Copyright   : (c) Alias Qli, 2022
License     : BSD-3-Clause
Maintainer  : 2576814881@qq.com
Stability   : experimental
Portability : POSIX

This module implements continuation-based I/O as described in Haskell Report 1.2. It uses 
the same basic types as the stream-based I/O.
-}

module System.IO.Continuation
  ( -- * Continuation Types
    SuccCont
  , StrCont
  , StrListCont
  , BinCont
  , FailCont
  , -- * Transactions
    -- | Continuation-based I/O is based on a collection of functions called /transactions/
    -- defined in a continuation style. Please refer to the corresponding constructors under 
    -- 'Request' for documentations.
    done
  , readFile
  , writeFile
  , appendFile
  , readBinFile
  , writeBinFile
  , appendBinFile
  , deleteFile
  , statusFile
  , readChan
  , appendChan
  , readBinChan
  , appendBinChan
  , statusChan
  , echo
  , getArgs
  , getProgName
  , getEnv
  , setEnv
  , -- * Other Functions
    exit
  , abort
  , print
  , prints
  , interact
  , -- * Re-exports
    -- ** Types
    Dialogue
  , Bin
  , Name
  , IOError (..)
  , -- ** Channels
    stdin
  , stdout
  , stderr
  , stdecho
  , -- ** Run the Program
    runDialogue
  ) where

import           Prelude            (Bool, Show (..), String, shows)
import           System.IO.Dialogue

type SuccCont = Dialogue
type StrCont = String -> Dialogue
type StrListCont = [String] -> Dialogue
type BinCont = Bin -> Dialogue
type FailCont = IOError -> Dialogue

strDispatch :: FailCont -> StrCont -> Dialogue
strDispatch fail succ (resp:resps) = case resp of
  Str val     -> succ val resps
  Failure msg -> fail msg resps

strListDispatch :: FailCont -> StrListCont -> Dialogue
strListDispatch fail succ (resp:resps) = case resp of
  StrList val -> succ val resps
  Failure msg -> fail msg resps

binDispatch :: FailCont -> BinCont -> Dialogue
binDispatch fail succ (resp:resps) = case resp of
  Bn val      -> succ val resps
  Failure msg -> fail msg resps

succDispatch :: FailCont -> SuccCont -> Dialogue
succDispatch fail succ (resp:resps) = case resp of
  Success     -> succ resps
  Failure msg -> fail msg resps

done :: Dialogue
done _ = []

readFile  :: Name -> FailCont -> StrCont -> Dialogue
readFile name fail succ resps = ReadFile name : strDispatch fail succ resps

writeFile :: Name -> String -> FailCont -> SuccCont -> Dialogue
writeFile name contents fail succ resps = WriteFile name contents : succDispatch fail succ resps

appendFile :: Name -> String -> FailCont -> SuccCont -> Dialogue
appendFile name contents fail succ resps = AppendFile name contents : succDispatch fail succ resps

readBinFile  :: Name -> FailCont -> BinCont -> Dialogue
readBinFile name fail succ resps = ReadBinFile name : binDispatch fail succ resps

writeBinFile :: Name -> Bin -> FailCont -> SuccCont -> Dialogue
writeBinFile name contents fail succ resps = AppendBinFile name contents : succDispatch fail succ resps

appendBinFile :: Name -> Bin -> FailCont -> SuccCont -> Dialogue
appendBinFile name contents fail succ resps = AppendBinFile name contents : succDispatch fail succ resps

deleteFile :: Name -> FailCont -> SuccCont -> Dialogue
deleteFile name fail succ resps = DeleteFile name : succDispatch fail succ resps

statusFile :: Name -> FailCont -> StrCont -> Dialogue
statusFile name fail succ resps = StatusFile name : strDispatch fail succ resps

readChan :: Name -> FailCont -> StrCont -> Dialogue
readChan name fail succ resps = ReadChan name : strDispatch fail succ resps

appendChan :: Name -> String -> FailCont -> SuccCont -> Dialogue
appendChan name contents fail succ resps = AppendChan name contents : succDispatch fail succ resps

readBinChan :: Name -> FailCont -> BinCont -> Dialogue
readBinChan name fail succ resps = ReadBinChan name : binDispatch fail succ resps

appendBinChan :: Name -> Bin -> FailCont -> SuccCont -> Dialogue
appendBinChan name contents fail succ resps = AppendBinChan name contents : succDispatch fail succ resps

statusChan :: Name -> FailCont -> StrCont -> Dialogue
statusChan name fail succ resps = StatusChan name : strDispatch fail succ resps

echo :: Bool -> FailCont -> SuccCont -> Dialogue
echo bool fail succ resps = Echo bool : succDispatch fail succ resps

getArgs :: FailCont -> StrListCont -> Dialogue
getArgs fail succ resps = GetArgs : strListDispatch fail succ resps

getProgName :: FailCont -> StrCont -> Dialogue
getProgName fail succ resps = GetProgName : strDispatch fail succ resps

getEnv :: String -> FailCont -> StrCont -> Dialogue
getEnv name fail succ resps = GetEnv name : strDispatch fail succ resps

setEnv :: String -> String -> FailCont -> SuccCont -> Dialogue
setEnv name value fail succ resps = SetEnv name value : succDispatch fail succ resps

abort :: FailCont
abort _ = done

exit :: FailCont
exit err = appendChan stderr msg abort done
  where
    msg = case err of
      ReadError s   -> s
      WriteError s  -> s
      SearchError s -> s
      FormatError s -> s
      OtherError s  -> s

print :: Show a => a -> Dialogue
print x = appendChan stdout (show x) exit done

prints :: Show a => a -> String -> Dialogue
prints x s = appendChan stdout (shows x s) exit done

interact :: (String -> String) -> Dialogue
interact f = readChan stdin exit
  (\x -> appendChan stdout (f x) exit done)
