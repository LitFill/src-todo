{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.Commands (run) where

import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Bool
import qualified Data.Maybe
import qualified Flow
import qualified Options.Applicative
import qualified Text.Printf

import Todo.IO
import Todo.Types

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

run :: IO ()
run = do
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

