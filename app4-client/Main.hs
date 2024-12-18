module Main (main) where

import Data.ByteString (ByteString)
import Network.Wreq
import Data.String.Conversions
import Control.Lens
import System.IO (putStr, hFlush, stdout)

main :: IO ()
main = do
    putStrLn "-- BOOK STORE v0.20 --"
    putStrLn "Enter command (or 'quit' to exit):"
    putStrLn "(Use ':paste' to enter multiple commands)"
    mainLoop

mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout
    cmd <- getLine
    if 		cmd == "quit" 	then putStrLn "Exiting client..."
    else if cmd == ":paste" then handlePasteMode
    else do
        let rawRequest = cs cmd :: ByteString
        resp <- post "http://localhost:3000" rawRequest
        putStrLn $ cs $ resp ^. responseBody
        mainLoop

handlePasteMode :: IO ()
handlePasteMode = do
    putStrLn "Enter multiple commands (press Ctrl-D or an empty line to finish):"
    pasteCommands []

pasteCommands :: [String] -> IO ()
pasteCommands accCommands = do
    putStr "paste> "
    hFlush stdout
    cmd <- getLine
    if null cmd then sendBatchCommands (reverse accCommands)
    else pasteCommands (cmd : accCommands)

sendBatchCommands :: [String] -> IO ()
sendBatchCommands commands = do
    let batchCommand = unlines commands
        rawRequest = cs batchCommand :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody
    mainLoop