{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Web.Scotty
import Control.Concurrent (Chan, newChan, writeChan, readChan, forkIO)
import Control.Concurrent.STM (newTVarIO, TVar)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Lib2 qualified
import Lib3 qualified
import Control.Monad.Trans.Except (runExceptT)
import qualified Control.Concurrent.STM as STM

-- Application state 
data AppState = AppState 
    { storageChan :: Chan Lib3.StorageOp
    , stateVar :: TVar Lib2.State
    }

main :: IO ()
main = do
    ioChan <- newChan :: IO (Chan Lib3.StorageOp)
    initialState <- newTVarIO Lib2.emptyState
    let appState = AppState ioChan initialState
    _ <- forkIO $ Lib3.storageOpLoop ioChan
    putStrLn "Starting HTTP server on port 3000..."
    scotty 3000 $ do
        post "/" $ handleCommand appState

handleCommand :: AppState -> ActionM ()
handleCommand appState = do
    reqBody <- body
    let cmdInput = cs reqBody :: String
    result <- liftIO $ runExceptT $ Lib3.parseCommand cmdInput
    case result of
        Left err -> do
            liftIO $ putStrLn $ "Error parsing command: " ++ err
            text (cs ("Invalid command: " ++ err))
        Right (cmd, _) -> do
            result <- liftIO $ executeLib3Command appState cmd
            text (cs result)

executeLib3Command :: AppState -> Lib3.Command -> IO String
executeLib3Command appState cmd = do
    case cmd of
        Lib3.SaveCommand -> do
            result <- Lib3.stateTransition (stateVar appState) cmd (storageChan appState)
            case result of
                Left err -> return $ "Save failed: " ++ err
                Right (Just msg, _) -> return msg
                Right (Nothing, _) -> return "State saved successfully."
        
        Lib3.LoadCommand -> do
            result <- Lib3.stateTransition (stateVar appState) cmd (storageChan appState)
            case result of
                Left err -> return $ "Load failed: " ++ err
                Right (Just msg, stateStr) -> return $ msg ++ " - " ++ stateStr
                Right (Nothing, stateStr) -> return $ "State loaded: " ++ stateStr
        
        Lib3.StatementCommand statements -> do
            result <- STM.atomically $ Lib3.executeStatements (stateVar appState) statements
            case result of
                Left err -> return $ "Command execution failed: " ++ err
                Right (msg, stateStr) -> 
                    return $ "Statements executed. \n" ++ 
                             (maybe "" (\m -> m ++ " ") msg) ++ 
                             "Current state: \n" ++ stateStr