module Main where

import Lib
import System.IO
import Data.Maybe

defaultFileName = "example.core"

readF :: String -> IO String
readF filename = do
    inh <- openFile filename ReadMode
    prog <- readloop inh
    hClose inh
    return prog

main :: IO (Program Name)
main = do
    putStr $ "Insert source filename [" ++ defaultFileName ++ "]: "
    hFlush stdout
    fileName <- getLine
    -- if fileName is empty, use defaultFileName
    program <- readF $ fromMaybe defaultFileName $ Just fileName
    let result = comp $ parse parseProgr program
    print result
    return result

comp :: [(CoreProgram, Name)] -> CoreProgram
comp []       = error "no parse"
comp [(e,[])] = e
comp [(_,a)]  = error ("doesn't use all input" ++ a)

readloop :: Handle -> IO String
readloop inh = do
    ineof <- hIsEOF inh
    if ineof
      then return []
      else do
        x <- hGetLine inh
        xs <- readloop inh
        return (x ++ xs)
