{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Web.Scotty
import Control.Concurrent (Chan, newChan, writeChan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Lib3 qualified
import System.IO (readFile, writeFile)

-- Application state: a shared channel for communication
type AppState = Chan Lib3.StorageOp

main :: IO ()
main = do
    -- Initialize shared channel for handling save/load operations
    ioChan <- newChan :: IO AppState
    putStrLn "Starting HTTP server on port 3000..."
    scotty 3000 $ do
        post "/" $ handleCommand ioChan

-- Handle incoming HTTP requests
handleCommand :: AppState -> ActionM ()
handleCommand ioChan = do
    -- Read the request body and convert it into a String
    reqBody <- body
    let cmdInput = cs reqBody :: String

    -- Parse the command using Lib3.parseCommand
    case Lib3.parseCommand cmdInput of
		Left err -> do
			liftIO $ putStrLn $ "Error parsing command: " ++ err
			text (cs ("Invalid command: " ++ err))
		Right (cmd, _) -> do  -- Extract the command, ignore the remaining String
			result <- liftIO $ executeLib3Command ioChan cmd
			text (cs result)


-- Execute a Lib3.Command parsed by Lib3.parseCommand
executeLib3Command :: AppState -> Lib3.Command -> IO String
executeLib3Command ioChan cmd =
    case cmd of
        Lib3.SaveCommand -> do
            -- Simulate save operation
            responseChan <- newChan
            writeChan ioChan (Lib3.Save "state" responseChan)
            _ <- readChan responseChan
            return "State saved successfully."

        Lib3.LoadCommand -> do
            -- Simulate load operation
            responseChan <- newChan
            writeChan ioChan (Lib3.Load responseChan)
            loadedContent <- readChan responseChan
            return $ "Loaded content: " ++ loadedContent

        Lib3.StatementCommand statements -> do
            -- Render and display statements
            let rendered = Lib3.renderStatements statements
            return $ "Executed statements: " ++ rendered
