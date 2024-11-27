module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [ "add", 
                "remove",
                "list",

				-- Genre Selection
				"Fiction", "Non-Fiction", "Mystery", "Thriller", "Romance", "Sci-Fi", "Fantasy", "Biography"
				]