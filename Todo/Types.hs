{-# LANGUAGE MultilineStrings    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}

module Todo.Types (
    Todo     (..),
    Location (..),

    noIdAndLocTodo,
    noLocTodo,

    showTodo,
    loc2string,

    addLoc,
    hasId,

    displayTodo,
    displayTodoCompact,
    ) where

import qualified Data.Char
import qualified Data.Maybe
import qualified Flow
import qualified Text.Printf

----------------------------------------
--- * Types
----------------------------------------

-- | Represents a specific location in a file.
data Location = Location
    { locFilePath   :: FilePath  -- ^ The path to the file.
    , locLineNumber :: Int       -- ^ 1-based line number.
    } deriving (Eq, Show)

-- | Formats a 'Location' into a 'String'.
--
-- >>> loc2string (Location "app/Main.hs" 10)
-- "app/Main.hs:10"
loc2string :: Location -> String
loc2string Location {..} =
    Text.Printf.printf
        "%s:%d"
        locFilePath
        locLineNumber

-- | The type alias for 'Todo' id.
type TodoID = String

-- | The core data type representing a single TODO item.
data Todo = Todo
    { todoId     :: Maybe TodoID     -- ^ Optional unique identifier, typically a UUID.
    , todoPrefix :: String           -- ^ Text before @TODO:@ keyword.
    , todoSuffix :: String           -- ^ Text after @TODO:@ keyword.
    , todoLoc    :: Maybe Location   -- ^ Optional location of the TODO.
    } deriving (Eq, Show)

-- | Converts a 'Todo' back into its source code 'String' representation.
-- This is used for persisting the 'Todo' back to a file.
--
-- >>> showTodo (Todo (Just "#8") "-- " "refactor this." (Just (Location "app/Main.hs" 42)))
-- "-- TODO: (#8) refactor this."
--
-- >>> showTodo (Todo Nothing "-- " "refactor this." (Just (Location "app/Main.hs" 42)))
-- "-- TODO: refactor this."
showTodo :: Todo -> String
showTodo Todo {..} =
    case todoId of
        Just tid ->
            Text.Printf.printf "%sTODO: (%s) %s" todoPrefix tid todoSuffix
        Nothing  ->
            Text.Printf.printf "%sTODO: %s"      todoPrefix     todoSuffix

-- | Creates a detailed, multi-line 'String' representation of a 'Todo'.
-- Ideal for showing detailed information about a single item.
-- Returns an error message if the 'Todo' has no 'Location'.
--
-- >>> lines $ displayTodo (Todo (Just "#8") "-- " "refactor this." (Just (Location "app/Main.hs" 42)))
-- [ "# Todo",
--   "  - note     : refactor this.",
--   "  - id       : #8",
--   "  - location : app/Main.hs:42" ]
displayTodo :: Todo -> String
displayTodo Todo {..} = maybe err go todoLoc
  where
    go = loc2string Flow..> Text.Printf.printf
        """
        # Todo
          - note     : %s
          - id       : %s
          - location : %s
        """
        suff tid
    suff = dropWhile Data.Char.isSpace todoSuffix
    tid  = Data.Maybe.fromMaybe "" todoId
    err  = "[ERROR] Cannot display a todo with no location."

-- | Creates a compact, single-line 'String' representation of a 'Todo'.
-- Useful for listing multiple items.
-- Returns an error message if the 'Todo' has no 'Location'.
--
-- >>> displayTodoCompact (Todo (Just "#8") "-- " "refactor this." (Just (Location "app/Main.hs" 42)))
-- "app/Main.hs:42 : (#8) refactor this."
displayTodoCompact :: Todo -> String
displayTodoCompact Todo {..} = maybe err go todoLoc
  where
    go loc = Text.Printf.printf
        "%s : %s%s"
        (loc2string loc) tid suff
    suff = dropWhile Data.Char.isSpace todoSuffix
    tid  = maybe "" (Text.Printf.printf "(%s) ") todoId
    err  = "[ERROR] Cannot display a todo with no location."

-- | A smart constructor for a 'Todo' that has no 'Location' information yet.
--
-- >>> noLocTodo "#123" "  // " "refactor this."
-- Todo {todoId = Just "#123", todoPrefix = "  // ", todoSuffix = "refactor this.", todoLoc = Nothing}
noLocTodo
    :: TodoID  -- ^ the Todo Id.
    -> String  -- ^ text before the "TODO:".
    -> String  -- ^ text after the "TODO:".
    -> Todo
noLocTodo (pure -> tid) pref suff =
    Todo tid pref suff Nothing

-- | A smart constructor for a 'Todo' that has neither an ID nor a 'Location'.
--
-- >>> noIdAndLocTodo "  // " "refactor this."
-- Todo {todoId = Nothing, todoPrefix = "  // ", todoSuffix = "refactor this.", todoLoc = Nothing}
noIdAndLocTodo
    :: String  -- ^ text before the "TODO:".
    -> String  -- ^ text after the "TODO:".
    -> Todo
noIdAndLocTodo pref suff =
    Todo Nothing pref suff Nothing

-- | Adds (or replaces) 'Location' information to an existing 'Todo'.
--
-- >>> addLoc "./src/Lib.hs" 42 (Todo (Just "#69") "-- " "use the other library." Nothing)
-- Todo {todoId = Just "#69", todoPrefix = "-- ", todoSuffix = "use the other library.", todoLoc = Just (Location {locFilePath = "./src/Lib.hs", locLineNumber = 42})}
--
-- >>> addLoc "./src/Lib.hs" 42 (Todo (Just "#69") "-- " "use the other library." (Just (Location "Lib.hs" 54)))
-- Todo {todoId = Just "#69", todoPrefix = "-- ", todoSuffix = "use the other library.", todoLoc = Just (Location {locFilePath = "./src/Lib.hs", locLineNumber = 42})}
addLoc
    :: FilePath  -- ^ the file path.
    -> Int       -- ^ the line number, 1-based.
    -> Todo      -- ^ the 'Todo' to modify.
    -> Todo
addLoc f l t = t {todoLoc = Location f l Flow.|> Just}

-- | A predicate to check if a 'Todo' has a specific ID.
hasId :: TodoID -> Todo -> Bool
hasId (Just -> tid) t = todoId t == tid

