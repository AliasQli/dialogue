{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import           Prelude                hiding (readFile)
import           System.IO.Continuation
import           System.IO.Dialogue

example1 :: Dialogue
example1 ~(Success: ~((Str userInput) : ~(Success : ~(r4 : _)))) =
  [ AppendChan stdout "please type a filename\n"
  , ReadChan stdin
  , AppendChan stdout name
  , ReadFile name
  , AppendChan stdout (case r4 of Str contents    -> contents
                                  Failure ioerror -> "can't open file")
  , AppendChan stdout "\nfinished"
  ] where (name : _) = lines userInput

example2 :: Dialogue
example2 =
  appendChan stdout "please type a filename\n" exit (
  readChan stdin exit (\userInput ->
  let (name : _) = lines userInput in
  appendChan stdout name exit (
  readFile name (\ioerror -> appendChan stdout
                             "can't open file" exit done)
                (\contents ->
  appendChan stdout contents exit done))))


program :: Dialogue
program = readChan stdin exit (\userInput -> readNums (lines userInput))

readNums :: [String] -> Dialogue
readNums inputLines =
  readInt "Enter first number: " inputLines
    (\num1 inputLines1 ->
      readInt "Enter second number: " inputLines1
        (\num2 _ -> reportResult num1 num2))

reportResult :: Int -> Int -> Dialogue
reportResult num1 num2 =
  appendChan stdout ("Their sum is: " ++ show (num1 + num2)) exit done

readInt :: String -> [String] -> (Int -> [String] -> Dialogue) -> Dialogue
readInt prompt inputLines succ =
  appendChan stdout prompt exit
    (case inputLines of
      (l1 : rest) -> case reads l1 of
        [(x,"")] -> succ x rest
        _        -> appendChan stdout
            "Error - retype the number\n" exit
            (readInt prompt rest succ)
      _ -> appendChan stdout "Early EOF" exit done)

main :: IO ()
main = runDialogue program
