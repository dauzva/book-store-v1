module Main (main) where

import Data.ByteString (ByteString)
import Network.Wreq
import Data.String.Conversions
import Control.Lens
import System.IO (putStr, hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Book Store Client"
    putStrLn "Enter command (or 'quit' to exit):"
    mainLoop

mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout
    cmd <- getLine
    if cmd == "quit"
    then putStrLn "Exiting client..."
    else do
        let rawRequest = cs cmd :: ByteString
        resp <- post "http://localhost:3000" rawRequest
        putStrLn $ cs $ resp ^. responseBody
        mainLoop