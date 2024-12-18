{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
	Parser,
	parse
    ) where
import qualified Data.Char as C
import qualified Data.List as L
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (runState)

validGenres :: [String]
validGenres = [
    "Fiction",
    "Non-Fiction",
    "Mystery",
    "Thriller",
    "Romance",
    "Sci-Fi",
    "Fantasy",
    "Biography"
    ]

-- Run a Parser
type Parser a = ExceptT String (S.State String) a
parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = 
    AddQuery String |
    RemoveQuery String |
    ListQuery

-- | The instances are needed basically for tests
instance Eq Query where
    (==) ListQuery ListQuery = True
    (==) (AddQuery a) (AddQuery b) = a == b
    (==) (RemoveQuery a) (RemoveQuery b) = a == b
    (==) _ _ = False

instance Show Query where
    show (AddQuery a)       = show a
    show (RemoveQuery a)    = show a
    show ListQuery          = show "List:"

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input = 
    case parse fullParser input of
        (Left err, _) -> Left err
        (Right result, _) -> Right result
  where
    fullParser :: Parser Query
    fullParser = do
        cmd <- parseCmd
        parseBook cmd


parseComma :: Parser ()
parseComma = do
    input <- lift S.get
    let (spaces1, rest1) = span C.isSpace input
        (comma, rest2) = splitAt 1 rest1
        (_, rest3) = span C.isSpace rest2
    if comma == ","
        then lift $ S.put rest3
        else throwE "Invalid separator"


parseCmd :: Parser Query
parseCmd = do
    input <- lift S.get
    let (cmd, rest1) = span C.isAlpha input
        rest2 = dropWhile C.isSpace rest1
    case cmd of
        "add" -> do
            lift $ S.put rest2
            return $ AddQuery rest2
        "remove" -> do
            lift $ S.put rest2
            return $ RemoveQuery rest2
        "list" -> do
            lift $ S.put rest2
            return ListQuery
        _ -> throwE "Invalid command"



parseBook :: Query -> Parser Query
parseBook ListQuery = return ListQuery
parseBook (AddQuery _) = do
    title <- parseTitle
    author <- parseAuthor
    genre <- parseGenre
    year <- parseYear
    price <- parsePrice
    return $ AddQuery (title ++ ", " ++ author ++ ", " ++ genre ++ ", " ++ year ++ ", " ++ price)
parseBook (RemoveQuery _) = do
    idStr <- parseId
    return $ RemoveQuery idStr


parseTitle :: Parser String
parseTitle = do
    input <- lift S.get
    case input of
        ('"':xs) -> do
            let (title, rest) = break (== '"') xs
            if null rest
                then throwE "Invalid Title syntax"
                else do
                    lift $ S.put (drop 1 rest)
                    return title
        _ -> throwE "Invalid Title syntax"


parseAuthor :: Parser String
parseAuthor = do
    parseComma
    input <- lift S.get
    let (name, rest1) = span C.isAlpha input
        (spaces, rest2) = span C.isSpace rest1
        (surname, rest3) = span C.isAlpha rest2
    if null name || null surname
        then throwE "Invalid Author"
        else do
            lift $ S.put rest3
            return $ name ++ " " ++ surname


parseGenre :: Parser String
parseGenre = do
    parseComma
    input <- lift S.get
    let (genre, rest) = span (\c -> C.isAlpha c || c == '-') input
    if genre `elem` validGenres
        then do
            lift $ S.put rest
            return genre
        else throwE "Invalid Genre"

					
parseYear :: Parser String
parseYear = do
    parseComma
    input <- lift S.get
    let (year, rest) = splitAt 4 input
    if all C.isDigit year && length year == 4
        then do
            lift $ S.put rest
            return year
        else throwE "Invalid Year"


parsePrice :: Parser String
parsePrice = do
    parseComma
    input <- lift S.get
    let (pricePart1, rest1) = span C.isNumber input
        (decimal, rest2) = splitAt 1 rest1
        (pricePart2, rest3) = span C.isNumber rest2
    case (pricePart1, decimal, pricePart2) of
        (_, ".", []) -> throwE "Invalid Price"
        ([], _, _) -> throwE "Invalid Price"
        (_, ".", p2) | length p2 > 2 -> throwE "Invalid Price"
        (_, ".", _) -> do
            lift $ S.put rest3
            return $ pricePart1 ++ decimal ++ pricePart2
        (_, _, _) -> do
            lift $ S.put rest1
            return pricePart1


parseId :: Parser String
parseId = do
    input <- lift S.get
    let (spaces, rest1) = span C.isSpace input
        (idStr, rest2) = span C.isDigit rest1
    if null idStr
        then throwE "Invalid ID"
        else do
            lift $ S.put rest2
            return idStr

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State [Query]
instance Show State where
    show (State queries) = 
        let formatQuery (AddQuery a) = a
            formatQuery (RemoveQuery a) = a
            formatQuery ListQuery = "List:"
        in unlines (map formatQuery queries)


-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State old) q = 
    case q of
        ListQuery ->
            Right (Just (show (State old)), State old)
        AddQuery a ->
            let
                id = show (length old + 1)
                newQuery = AddQuery (id ++ ". " ++ a)
                newState = State (old ++ [newQuery])
            in
                Right (Just (show newQuery), newState)
        RemoveQuery idStr ->
            let
                id = read idStr :: Int
                isValidIndex = id >= 1 && id <= length old
                matchId (AddQuery str) = takeWhile (/= '.') str == idStr
                newList = filter (not . matchId) old
                reindexed = zipWith reindex [1..] newList
                reindex n (AddQuery str) =
                    let content = dropWhile (/= '.') str
                    in AddQuery (show n ++ content)
                newState = State reindexed
            in
                if not isValidIndex 
                then Left "Id not found"
                else Right (Just "Entry removed", newState)
