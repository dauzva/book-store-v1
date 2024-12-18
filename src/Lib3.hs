{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
	executeStatements,
    Statements(..),
    Command(..)
    ) where

import Control.Concurrent ( Chan, writeChan, readChan )
import Control.Concurrent.STM( STM, TVar, atomically, readTVar, writeTVar, readTVarIO )
import Control.Monad ( when )
import System.IO ( withFile, IOMode(ReadMode, WriteMode), hPutStrLn, hGetContents )
import qualified Lib2
import Control.Concurrent.Chan (newChan)
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad (forM_)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop ioChan = do
    op <- readChan ioChan
    case op of
        Save content responseChan -> do
            handle <- openFile "state.txt" WriteMode
            hPutStrLn handle content
            writeChan responseChan ()
            hClose handle
        Load responseChan -> do
            handle <- openFile "state.txt" ReadMode
            content <- hGetContents handle
            writeChan responseChan content
    storageOpLoop ioChan

data Statements = Batch [Lib2.Query] |
                 Single Lib2.Query
                 deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> ExceptT String IO (Command, String)
parseCommand str
    | str == "load" = return (LoadCommand, "")
    | str == "save" = return (SaveCommand, "")
    | otherwise = do
        (statements, rest) <- parseStatements str
        return (StatementCommand statements, rest)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> ExceptT String IO (Statements, String)
parseStatements str = do
    let trimmed = dropWhile (== ' ') str
        linesOfInput = lines trimmed
    parseQueries linesOfInput []

  where
    parseQueries :: [String] -> [Lib2.Query] -> ExceptT String IO (Statements, String)
    parseQueries [] acc = return (Batch (reverse acc), "")
    parseQueries (line:rest) acc
        | null (dropWhile (== ' ') line) = parseQueries rest acc
        | otherwise = do
            query <- case Lib2.parseQuery line of
                Left err -> throwE $ "Error parsing query: " ++ err
                Right query -> return query
            parseQueries rest (query : acc)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State queries) = Batch queries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderStatements (Batch [query])
renderStatements (Batch queries) = unlines $ map getStringFromQuery queries

getStringFromQuery :: Lib2.Query -> String
getStringFromQuery q = do
  case q of
    Lib2.AddQuery a ->
        let
            dropid = drop 3 a
            title = takeWhile (/= ',') dropid
            rest = drop (length title) dropid
        in
            "add " ++ "\"" ++ title ++ "\"" ++ rest


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String, String))
stateTransition stateVar command ioChan =
    case command of
        LoadCommand -> do
            responseChan <- newChan
            writeChan ioChan (Load responseChan)
            content <- readChan responseChan
            result <- runExceptT $ parseStatements content
            case result of
                Left err -> return $ Left err
                Right (Batch queries, _) -> do
                    atomically $ do
                        writeTVar stateVar Lib2.emptyState
                        executeBatch stateVar Lib2.emptyState queries
                    return $ Right (Just "State loaded", show queries)
                Right _ -> return $ Left "Unexpected non-batch command in loaded content."

        SaveCommand -> do
            currentState <- readTVarIO stateVar
            let stateStr = renderStatements (marshallState currentState)
            responseChan <- newChan
            writeChan ioChan (Save stateStr responseChan)
            readChan responseChan
            return $ Right (Just "State saved", show currentState)

        StatementCommand statements ->
            atomically $ executeStatements stateVar statements

executeStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String, String))
executeStatements stateVar statements = do
    currentState <- readTVar stateVar
    case statements of
        Single query ->
            case Lib2.stateTransition currentState query of
                Left err -> return $ Left err
                Right (msg, newState) -> do
                    writeTVar stateVar newState
                    return $ Right (msg, show newState)
        Batch queries ->
            executeBatch stateVar currentState queries


executeBatch :: TVar Lib2.State -> Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String, String))
executeBatch stateVar initialState [] = do
    writeTVar stateVar initialState
    return $ Right (Nothing, show initialState)
executeBatch stateVar initialState (q:qs) =
    case Lib2.stateTransition initialState q of
        Left err -> return $ Left err
        Right (_, newState) -> executeBatch stateVar newState qs
