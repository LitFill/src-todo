{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE MultilineStrings    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module      : Main
Description : A command-line tool for managing TODO comments in source code.

This module provides a simple yet effective way to find, list, register,
and manage TODO items embedded within text files. It can recursively
search directories, parse TODOs with and without unique IDs, and
persist changes back to the source files.
-}
module Main (main) where

import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Attoparsec.Text
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified Flow
import qualified Options.Applicative
import qualified System.Directory
import qualified System.Directory.Tree
import qualified System.IO
import qualified Text.Printf
import qualified Data.UUID.V4
import qualified Data.UUID

----------------------------------------
--- * Types
----------------------------------------

-- | Represents a specific location in a file.
data Location = Location
    { locFilePath   :: FilePath  -- ^ The path to the file.
    , locLineNumber :: Int       -- ^ 1-based line number.
    }

instance Show Location where show = loc2string

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

-- | The core data type representing a single TODO item.
data Todo = Todo
    { todoId     :: Maybe String     -- ^ Optional unique identifier, typically a UUID.
    , todoPrefix :: String           -- ^ Text before @TODO:@ keyword.
    , todoSuffix :: String           -- ^ Text after @TODO:@ keyword.
    , todoLoc    :: Maybe Location   -- ^ Optional location of the TODO.
    }

instance Show Todo where show = showTodo

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
    tid  = maybe "" (Text.Printf.printf "(%s) ") todoId :: String
    err  = "[ERROR] Cannot display a todo with no location."

-- | A smart constructor for a 'Todo' that has no 'Location' information yet.
-- params: id, prefix, suffix
noLocTodo :: String -> String -> String -> Todo
noLocTodo (pure -> id') pref suff =
    Todo id' pref suff Nothing

-- | A smart constructor for a 'Todo' that has neither an ID nor a 'Location'.
-- params: prefix, suffix
noIdAndLocTodo :: String -> String -> Todo
noIdAndLocTodo pref suff =
    Todo Nothing pref suff Nothing

-- | Adds (or replaces) 'Location' information to an existing 'Todo'.
addLoc :: FilePath -> Int -> Todo -> Todo
addLoc f l t = t {todoLoc = Location f l Flow.|> Just}

-- | A predicate to check if a 'Todo' has a specific ID.
hasId :: String -> Todo -> Bool
hasId (Just -> tid) t = t.todoId == tid

----------------------------------------
--- * Parser
----------------------------------------

type Parser = Data.Attoparsec.Text.Parser

withIdTodoP :: Parser Todo
withIdTodoP = do
    (pref, hasTodo) <- parsePrefixCI
    if not hasTodo then fail "No TODO found" else do
        _    <- Data.Attoparsec.Text.skipSpace
             *> Data.Attoparsec.Text.char ':'
             <* Data.Attoparsec.Text.skipSpace
        id'  <- Data.Attoparsec.Text.char '('
             *> Data.Attoparsec.Text.takeWhile1 (/= ')')
             <* Data.Attoparsec.Text.char ')'
             <* Data.Attoparsec.Text.skipSpace
        suff <- Data.Attoparsec.Text.manyTill
                Data.Attoparsec.Text.anyChar
                Data.Attoparsec.Text.endOfInput
        pure $ noLocTodo (Data.Text.unpack id') pref suff

noIdTodoP :: Parser Todo
noIdTodoP = do
    (pref, hasTodo) <- parsePrefixCI
    if not hasTodo then fail "No TODO found" else do
        _    <- Data.Attoparsec.Text.skipSpace
             *> Data.Attoparsec.Text.char ':'
             <* Data.Attoparsec.Text.skipSpace
        suff <- Data.Attoparsec.Text.manyTill
                Data.Attoparsec.Text.anyChar
                Data.Attoparsec.Text.endOfInput
        pure $ noIdAndLocTodo pref suff

-- | Parse a string case-insensitively.

todoP :: Parser Todo
todoP = withIdTodoP Control.Applicative.<|> noIdTodoP

-- >>> Data.Attoparsec.Text.parseOnly (inParens (Data.Attoparsec.Text.manyTill Data.Attoparsec.Text.anyChar (Data.Attoparsec.Text.char ')'))) "(hello) world"
-- Left "')': Failed reading: satisfy"

-- >>> Data.Attoparsec.Text.parseOnly (Data.Attoparsec.Text.char '(' *> Data.Attoparsec.Text.takeWhile1 (/= ')') <* Data.Attoparsec.Text.char ')') "(123 fix this)"
-- Right "123 fix this"

-- >>> Data.Attoparsec.Text.parseOnly withIdTodoP "    -- TODO: (123) fix this"
-- Right     -- TODO: (123) fix this

-- | Helper: parse prefix up to (case-insensitive) TODO, return (prefix, found?)
parsePrefixCI :: Parser (String, Bool)
parsePrefixCI = do
    prefix <- Data.Attoparsec.Text.takeWhile1 isNotT Control.Applicative.<|> pure ""
    rest   <- Data.Attoparsec.Text.take 4
    pure $ if Data.Text.toCaseFold rest == "todo"
        then (Data.Text.unpack prefix          , True)
        else (Data.Text.unpack (prefix <> rest), False)
  where
    isNotT c = c /= 'T' && c /= 't'

----------------------------------------
--- * File IO
----------------------------------------

-- | Equivalent to 'Data.Functor.<&>' but 'infixl 0'
infixl 0 ||>
(||>) :: Functor f => f a -> (a -> b) -> f b
a ||> f = f <$> a

-- | Reads a single file and extracts all 'Todo' items from it,
-- enriching each with its 'Location'.
extractTodos :: FilePath -> IO [Todo]
extractTodos fname = do
    content <- Data.Text.IO.readFile fname
    let ls = Data.Text.lines content
    let parsed = zip [1..] $ fmap parseLine ls
    pure $ Data.Maybe.mapMaybe (\(a, b) -> addLoc fname a <$> b) parsed
  where
    -- Helper to run the parser and return Maybe
    parseLine :: Data.Text.Text -> Maybe Todo
    parseLine t = case Data.Attoparsec.Text.parseOnly todoP t of
        Right todo -> Just todo
        Left _     -> Nothing

-- | Recursively reads a directory tree, finds all files, and extracts 'Todo'
-- items from them.
extractTodos' :: FilePath -> IO [Todo]
extractTodos' root = do
    (_ System.Directory.Tree.:/ tree) <-
        System.Directory.Tree.filterDir System.Directory.Tree.successful
        System.Directory.Tree.</$>
        System.Directory.Tree.readDirectoryWithL extractTodos root
    tree
        Flow.|> System.Directory.Tree.flattenDir
        Flow.|> filter isFile
        Flow.|> concatMap System.Directory.Tree.file
        Flow.|> pure
  where
    isFile System.Directory.Tree.File{} = True
    isFile _                            = False

-- | Given a list of file or directory paths, extracts all 'Todo's from them.
files2todos :: [FilePath] -> IO [Todo]
files2todos fnames =
    fnames
    Flow.|> traverse extractTodos'
        ||> concat

-- | Generate a new UUID as a String
--   (uses random IO)
genUUIDText :: IO String
genUUIDText = Data.UUID.toString <$> Data.UUID.V4.nextRandom

-- | Takes a 'Todo' and assigns a new, unique ID using UUID.
-- The returned 'Todo' will have a 'Just' value for its 'todoId'.
registerTodo :: Todo -> IO Todo
registerTodo todo = do
    uuid <- genUUIDText
    pure todo {todoId = Just uuid}

-- | Persists a 'Todo' to its source file. This function has the side effect
-- of modifying a file on disk. It overwrites the line specified in the 'Todo's
-- 'Location' with the new 'Todo' content. Returns the ID of the persisted 'Todo'.
persistTodo :: Todo -> IO String
persistTodo t = do
    let tid = Data.Maybe.fromMaybe "" t.todoId
    case t.todoLoc of
        Nothing -> putStrLn "[ERROR] Cannot persist TODO with no location."
        Just (Location f l) ->
            Control.Exception.catch @Control.Exception.IOException
                (replaceAtLine l f $ showTodo t)
                (Text.Printf.printf
                    "[ERROR] Failed to persist TODO in file: %s (%s)\n" f
                    . show)
    pure tid

-- | A low-level utility to replace the contents of a specific line in a file.
-- This operation is performed safely using a temporary file.
replaceAtLine :: Int -> FilePath -> String -> IO ()
replaceAtLine lnum fname (Data.Text.pack -> text) = do
    eres <- Control.Exception.try
           @Control.Exception.IOException
           (Data.Text.IO.readFile fname)
    case eres of
        Left (show -> err) -> Text.Printf.printf
            "[ERROR] Could not read file: %s (%s)" fname err
        Right content      -> process $ Data.Text.lines content
  where
    process ls
        | lnum <= 0 || lnum > length ls = Text.Printf.printf
            "[ERROR] replaceAtLine: line number %d is out of bounds in %s.\n"
            lnum fname
        | otherwise = do
            let (hd, tl) = ls Flow.|> splitAt (lnum - 1)
            let ls' = hd ++ [text] ++ drop 1 tl
            let f'  = Data.Text.unlines ls'
            eres <- Control.Exception.try
                   @Control.Exception.IOException
                   (System.IO.openTempFile "." ".src-todo-temp.txt")
            case eres of
                Left (show -> err) ->
                    putStrLn $ "[ERROR] Failed to open temp file: " ++ err
                Right (tmpFile, tmpHandle) -> do
                    Data.Text.IO.hPutStr tmpHandle f'
                    System.IO.hClose tmpHandle
                    Control.Exception.catch @Control.Exception.IOException
                        (System.Directory.renameFile tmpFile fname)
                        (Text.Printf.printf
                            "[ERROR] Could not rename temp file: %s\n"
                            . show)

----------------------------------------
--- * Commands
----------------------------------------

-- | Represents the set of command-line actions the program can perform.
data Command
    = Register                [FilePath]  -- ^ Finds Todos and assign IDs to new Todos.
    | Show String Bool        [FilePath]  -- ^ Show a specific TODO by its ID. The 'Bool' is for compact view.
    | List Bool               [FilePath]  -- ^ List all TODOs found. The 'Bool' is for compact view.
    | ReplaceId String String [FilePath]  -- ^ Replace an old ID with a new ID.
    | Unregister String       [FilePath]  -- ^ Remove a TODO by its ID.

-- | An 'Options.Applicative' parser for one or more file/directory paths.
files :: Options.Applicative.Parser [FilePath]
files =
    Options.Applicative.metavar "FILES..."
    Flow.|> Options.Applicative.argument Options.Applicative.str
    Flow.|> Options.Applicative.many

-- | 'Options.Applicative' parser for the 'Register' command.
register :: Options.Applicative.Parser Command
register = Register <$> files

-- | 'Options.Applicative' parser for the 'Unregister' command.
unregister :: Options.Applicative.Parser Command
unregister =
    Unregister
    <$> Options.Applicative.argument
        Options.Applicative.str
       (Options.Applicative.metavar "ID")
    <*> files

-- | 'Options.Applicative' parser for the 'Show' command.
show' :: Options.Applicative.Parser Command
show' =
    Show
    <$> Options.Applicative.argument
        Options.Applicative.str
       (Options.Applicative.metavar "ID")
    <*> Options.Applicative.switch
         ( Options.Applicative.long "compact"
        <> Options.Applicative.short 'c'
        <> Options.Applicative.help "Display in a compact format" )
    <*> files

-- | 'Options.Applicative' parser for the 'List' command.
list :: Options.Applicative.Parser Command
list =
    List
    <$> Options.Applicative.switch
         ( Options.Applicative.long "compact"
        <> Options.Applicative.short 'c'
        <> Options.Applicative.help "Display in a compact format" )
    <*> files

-- | 'Options.Applicative' parser for the 'ReplaceId' command.
replaceId :: Options.Applicative.Parser Command
replaceId =
    ReplaceId
    <$> Options.Applicative.argument
        Options.Applicative.str
       (Options.Applicative.metavar "OLD_ID")
    <*> Options.Applicative.argument
        Options.Applicative.str
       (Options.Applicative.metavar "NEW_ID")
    <*> files

-- | The main command-line options parser that combines all subcommands.
opts :: Options.Applicative.Parser Command
opts =
     Options.Applicative.subparser Flow.<|
        (       Options.Applicative.progDesc "Register new todos"
        Flow.|> Options.Applicative.info      register
        Flow.|> Options.Applicative.command  "register"   )
     <> (       Options.Applicative.progDesc "Unregister a todo by id"
        Flow.|> Options.Applicative.info      unregister
        Flow.|> Options.Applicative.command  "unregister" )
     <> (       Options.Applicative.progDesc "Show a todo by id"
        Flow.|> Options.Applicative.info      show'
        Flow.|> Options.Applicative.command  "show"       )
     <> (       Options.Applicative.progDesc "List all todos"
        Flow.|> Options.Applicative.info      list
        Flow.|> Options.Applicative.command  "list"       )
     <> (       Options.Applicative.progDesc "Replace a todo's id"
        Flow.|> Options.Applicative.info      replaceId
        Flow.|> Options.Applicative.command  "replace-id" )

-- | If the user provides no file paths, default to searching the current
-- directory, @["."]@. Otherwise, use the provided paths.
orDefault :: [FilePath] -> [FilePath]
orDefault [] = ["."]
orDefault fs = fs

-- | The main command handler. It takes a parsed 'Command' and executes the
-- corresponding IO actions.
handleCommand :: Command -> IO ()
handleCommand = \case
    Register fnames -> do
        todos <-
            files2todos (orDefault fnames)
            >>= traverse registerTodo
              . filter
              ( todoId Flow..> Data.Maybe.isNothing )
        if null todos then
            putStrLn "[INFO] No new todos found to register."
        else do
            ids <- unlines <$> traverse persistTodo todos
            Control.Monad.unless (null ids) do
                Text.Printf.printf
                    "Registered new todos with these ids:\n%s"
                    ids

    Unregister tid fnames -> do
        todos <- files2todos (orDefault fnames)
        let found = filter (hasId tid) todos
        if null found then
            putStrLn $ "[INFO] No TODO found with id: " ++ tid
        else
            Control.Monad.forM_ found $ \t -> do
                let t' = t { todoId = Nothing }
                _ <- persistTodo t'
                Text.Printf.printf "Unregistered id %s at %s\n"
                    tid (maybe "?" loc2string t.todoLoc)

    Show tid (isCompact -> display) fnames -> do
        todos <- files2todos (orDefault fnames)
        let found = filter (hasId tid) todos
        if null found then
            putStrLn $ "[INFO] No TODO found with id: " ++ tid
        else
            mapM_ (display Flow..> putStrLn) found

    List (isCompact -> display) fnames ->
        files2todos (orDefault fnames)
        >>= \todos -> if null todos
            then putStrLn "[INFO] No TODOs found."
            else mapM_ (display Flow..> putStrLn) todos

    ReplaceId oldId newId fnames -> do
        todos <- files2todos (orDefault fnames)
        let found = filter (hasId oldId) todos
        if null found then
            putStrLn $ "[INFO] No TODO found with id: " ++ oldId
        else
            Control.Monad.forM_ found \t -> do
                t {todoId = Just newId}
                    Flow.|> persistTodo
                    Flow.|> Control.Monad.void
                Text.Printf.printf
                    "The id %s is replaced with %s\n"
                    oldId newId
  where
    isCompact = Data.Bool.bool
        displayTodo
        displayTodoCompact

main :: IO ()
main = do
    eres <- Control.Exception.try
           @Control.Exception.IOException
           parseCLI
    case eres of
        Left  err ->
            putStrLn $ "[ERROR] Could not parse command line: " ++ show err
        Right cmd -> handleCommand cmd
  where
    parseCLI = Options.Applicative.execParser
             . Options.Applicative.info cli
             $ Options.Applicative.progDesc "A simple todo manager"
    cli = opts
        Options.Applicative.<**>
        Options.Applicative.helper
