{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE MultilineStrings    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns        #-}

module Main (main) where

import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Time
import qualified Data.Time.Format
import qualified Data.Void
import qualified Flow
import qualified Options.Applicative
import qualified System.Directory
import qualified System.Directory.Tree
import qualified System.IO
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Printf

----------------------------------------
-- Types
----------------------------------------

data Location = Location
    { locFilePath   :: FilePath
    , locLineNumber :: Int
    }

loc2string :: Location -> String
loc2string Location {..} =
    Text.Printf.printf
        "%s:%d"
        locFilePath
        locLineNumber

data Todo = Todo
    { todoId     :: Maybe String
    , todoPrefix :: String
    , todoSuffix :: String
    , todoLoc    :: Maybe Location
    }

showTodo :: Todo -> String
showTodo Todo {..} =
    case todoId of
        Just tid ->
            Text.Printf.printf "%sTODO: (%s) %s" todoPrefix tid todoSuffix
        Nothing  ->
            Text.Printf.printf "%sTODO: %s"      todoPrefix     todoSuffix

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

displayTodoCompact :: Todo -> String
displayTodoCompact Todo {..} = maybe err go todoLoc
  where
    go loc = Text.Printf.printf
        "%s : (%s) %s"
        (loc2string loc) tid suff
    suff = dropWhile Data.Char.isSpace todoSuffix
    tid  = Data.Maybe.fromMaybe "" todoId
    err  = "[ERROR] Cannot display a todo with no location."

noLocTodo :: String -> String -> String -> Todo
noLocTodo (pure -> id') pref suff =
    Todo id' pref suff Nothing

noIdAndLocTodo :: String -> String -> Todo
noIdAndLocTodo pref suff =
    Todo Nothing pref suff Nothing

addLoc :: FilePath -> Int -> Todo -> Todo
addLoc f l t = t {todoLoc = Location f l Flow.|> Just}

hasId :: String -> Todo -> Bool
hasId (Just -> tid) t = t.todoId == tid

----------------------------------------
-- Parser
----------------------------------------

type Parser = Text.Megaparsec.Parsec Data.Void.Void Data.Text.Text

inParens :: Parser a -> Parser a
inParens = Text.Megaparsec.between
    (Text.Megaparsec.Char.char '(')
    (Text.Megaparsec.Char.char ')')

withIdTodoP :: Parser Todo
withIdTodoP = do
    pref <- Text.Megaparsec.manyTill
              Text.Megaparsec.anySingle
            . Text.Megaparsec.lookAhead
            $ Text.Megaparsec.Char.string "TODO"
    _    <- Text.Megaparsec.Char.string' "todo"
    _    <- Text.Megaparsec.Char.space
             *> Text.Megaparsec.Char.char ':'
            <*  Text.Megaparsec.Char.space
    id'  <- inParens
            . Text.Megaparsec.manyTill
              Text.Megaparsec.anySingle
            . Text.Megaparsec.lookAhead
            $ Text.Megaparsec.Char.char ')'
    suff <- Text.Megaparsec.manyTill
            Text.Megaparsec.anySingle
            Text.Megaparsec.eof
    pure $ noLocTodo id' pref suff

noIdTodoP :: Parser Todo
noIdTodoP = do
    pref <-   Text.Megaparsec.manyTill
              Text.Megaparsec.anySingle
            $ Text.Megaparsec.lookAhead
            $ Text.Megaparsec.Char.string  "TODO"
    _    <-   Text.Megaparsec.Char.string' "todo"
    _    <-   Text.Megaparsec.Char.space
           *> Text.Megaparsec.Char.char ':'
           <* Text.Megaparsec.Char.space
    suff <-   Text.Megaparsec.manyTill
              Text.Megaparsec.anySingle
              Text.Megaparsec.eof
    pure $ noIdAndLocTodo pref suff

todoP :: Parser Todo
todoP = Text.Megaparsec.try withIdTodoP
        Text.Megaparsec.<|> noIdTodoP

----------------------------------------
-- File IOs
----------------------------------------

infixl 0 ||>
(||>) :: Functor f => f a -> (a -> b) -> f b
a ||> f = f <$> a

extractTodos :: FilePath -> IO [Todo]
extractTodos fname = do
    fname
    Flow.|> Data.Text.IO.readFile
        ||> Data.Text.lines
        ||> zip [1..]
        ||> fmap (fmap (Text.Megaparsec.parseMaybe todoP))
        ||> Data.Maybe.mapMaybe \(a, b) -> addLoc fname a <$> b

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

files2todos :: [FilePath] -> IO [Todo]
files2todos fnames =
    fnames
    Flow.|> traverse extractTodos'
        ||> concat

registerTodo :: Todo -> IO Todo
registerTodo todo = do
    time <- Data.Time.getCurrentTime
            ||> Data.Time.Format.formatTime
                Data.Time.Format.defaultTimeLocale
                "%Y%m%d%H%M%S%q"
    pure todo {todoId = time Flow.|> take 20 Flow.|> Just}

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
-- Commands
----------------------------------------

data Command
    = Register                [FilePath]
    | Show String Bool        [FilePath]
    | List Bool               [FilePath]
    | ReplaceId String String [FilePath]

files :: Options.Applicative.Parser [FilePath]
files =
    Options.Applicative.metavar "FILES..."
    Flow.|> Options.Applicative.argument Options.Applicative.str
    Flow.|> Options.Applicative.many

register :: Options.Applicative.Parser Command
register = Register <$> files

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

list :: Options.Applicative.Parser Command
list =
    List
    <$> Options.Applicative.switch
         ( Options.Applicative.long "compact"
        <> Options.Applicative.short 'c'
        <> Options.Applicative.help "Display in a compact format" )
    <*> files

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

opts :: Options.Applicative.Parser Command
opts =
    Options.Applicative.subparser Flow.<|
       (       Options.Applicative.progDesc "Register new todos"
       Flow.|> Options.Applicative.info      register
       Flow.|> Options.Applicative.command  "register"   )
    <> (       Options.Applicative.progDesc "Show a todo by id"
       Flow.|> Options.Applicative.info      show'
       Flow.|> Options.Applicative.command  "show"       )
    <> (       Options.Applicative.progDesc "List all todos"
       Flow.|> Options.Applicative.info      list
       Flow.|> Options.Applicative.command  "list"       )
    <> (       Options.Applicative.progDesc "Replace a todo's id"
       Flow.|> Options.Applicative.info      replaceId
       Flow.|> Options.Applicative.command  "replace-id" )

orDefault :: [FilePath] -> [FilePath]
orDefault [] = ["."]
orDefault fs = fs

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
