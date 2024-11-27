{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where
import qualified Data.Char as C
import qualified Data.List as L


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
parseQuery str = 
    case parseCmd str of
        Left(eCmd) -> Left(eCmd)
        Right(rCmd) -> 
			case parseBook rCmd of
				Left(eBook) -> Left(eBook)
				Right(rBook) -> Right(rBook)


parseComma :: String -> Either String String
parseComma str =
	let
		rest1 = L.takeWhile C.isSpace str
		rest2 = drop (length rest1) str
		comma = L.take 1 rest2
		rest3 = drop 1 rest2
		rest4 = L.takeWhile C.isSpace rest3
		rest = drop (length rest4) rest3
	in
		if comma == "," then Right rest
		else Left "Invalid separator"

parseCmd :: String -> Either String Query
parseCmd str =
        let
                cmd = L.takeWhile C.isAlpha str
                rest1 = drop (length cmd) str
                rest2 = L.takeWhile C.isSpace rest1
                rest = drop (length rest2) rest1
        in
                if      cmd == "add"    then Right (AddQuery rest)
                else if cmd == "remove" then Right (RemoveQuery rest)
                else if cmd == "list"   then Right (ListQuery)
                else                    Left "Invalid command"

parseBook :: Query -> Either String Query
parseBook ListQuery = Right ListQuery
parseBook (AddQuery str) = 
	case parseTitle str of
		Left(e1) -> Left(e1)
		Right(title, r1) ->
			case parseAuthor r1 of
				Left(e2) -> Left(e2)
				Right(author, r2) -> 
					case parseGenre r2 of
						Left(e3) -> Left(e3)
						Right(genre, r3) -> 
							case parseYear r3 of
								Left(e4) -> Left(e4)
								Right(year, r4) -> 
									case parsePrice r4 of
										Left(e5) -> Left(e5)
										Right(price, r5) -> Right(AddQuery $ title++", "++author++", "++genre++", "++year++", "++price)
parseBook (RemoveQuery str) =
	case parseId str of
		Left(e1) -> Left(e1)
		Right(r1) -> Right(RemoveQuery r1)

parseTitle :: String -> Either String (String, String)
parseTitle str = parseTitle' str []

parseTitle' :: String -> String -> Either String (String, String)
parseTitle' str [] = 
	let
		titleStart = L.take 1 str
		rest1 = drop 1 str
		title = L.takeWhile (/= '\"') rest1
		rest2 = drop (length title) rest1
		rest3 = L.takeWhile C.isSpace rest2
		rest = drop (length rest3) rest2
		titleEnd = L.take 1 rest
	in
		if (titleStart == "\"" && titleEnd == "\"") then parseTitle' rest title
		else Left "Invalid Title syntax"
parseTitle' str acc = 
	let
		titleStart = L.take 1 str
		rest1 = drop 1 str
		title = L.takeWhile (/= '\"') rest1
		rest2 = drop (length title) rest1
		rest3 = L.takeWhile C.isSpace rest2
		rest = drop (length rest3) rest2
		titleEnd = L.take 1 rest
	in
		if (titleStart == "\"" && titleEnd == "\"") then parseTitle' rest (acc++"\""++title)
		else Right (acc, title)

parseAuthor :: String -> Either String (String, String)
parseAuthor str =
	case parseComma str of
		Left(e1) -> Left(e1)
		Right(r1) -> 
			let
				name = L.takeWhile C.isAlpha r1
				rest1 = drop (length name) r1
				rest2 = L.takeWhile C.isSpace rest1
				rest3 = drop (length rest2) rest1
				surname = L.takeWhile C.isAlpha rest3
				rest = drop (length surname) rest3
			in
				if (name /= [] && surname /= []) then Right(name++" "++surname, rest)
				else Left "Invalid Author"

parseGenre :: String -> Either String (String, String)
parseGenre str = 
	case parseComma str of
		Left(e1) -> Left(e1)
		Right(r1) ->
			let
				genre = L.takeWhile (\c -> C.isAlpha c || c == '-') r1
				rest = drop (length genre) r1
			in
				if genre `elem` validGenres then Right (genre, rest)
				else Left "Invalid Genre"
					
parseYear :: String -> Either String (String, String)
parseYear str =
	case parseComma str of
		Left(e1) -> Left(e1)
		Right(r1) -> 
			let
				year = L.take 4 r1
				rest = drop (length year) r1
			in
				if all C.isDigit year then Right(year, rest)
				else Left "Invalid Year"

parsePrice :: String -> Either String (String, String)
parsePrice str =
	case parseComma str of
		Left(e1) -> Left(e1)
		Right(r1) -> 
			let
				price = L.takeWhile C.isNumber r1
				r2 = drop (length price) r1
				decimal = L.take 1 r2
				r3 = drop 1 r2
				price1 = L.takeWhile C.isNumber r3
				rest = drop (length price1) r3
			in
				if (price /= [] && (null decimal || all C.isSpace decimal) && (null rest || all C.isSpace rest)) then Right(price, rest)
				else if (price /= [] && decimal == "." && price1 /= [] && (length price1 == 1 || length price1 == 2) && (null rest || all C.isSpace rest)) then Right(price++"."++price1, rest)
				else Left "Invalid Price"

parseId :: String -> Either String String
parseId str =
	let
        r1 = L.takeWhile C.isSpace str
        r2 = drop (length r1) str
        id = L.takeWhile C.isNumber r2
	in
        if id /= [] then Right id
        else Left "Invalid ID"


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
                id = AddQuery ((show (length old + 1)) ++ ". " ++ a)
                new = State (old ++ [id])  
            in
                Right (Just (show new), new)
        RemoveQuery idStr ->
            let
                id = read idStr :: Int
                getId (AddQuery str) = takeWhile (/= '.') str
                matchId x = getId x == idStr
                newList = L.deleteBy (\x y -> matchId x && matchId y) (AddQuery idStr) old
                reindexed = zipWith reindex [1..] newList
                reindex n (AddQuery str) = 
                    let content = dropWhile (/= '.') str
                    in AddQuery (show n ++ content)
                new = State reindexed
            in
				if ((id > (length old)) || (id < 1)) then Left "Id not found"
                else Right (Just (show new), new)
		

