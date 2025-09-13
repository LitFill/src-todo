{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad    (forM_, unless, when, void)
import Data.Maybe       (fromJust, isNothing, mapMaybe, fromMaybe)
import Data.Text        (Text)
import Data.Time        (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Void        (Void)
import System.Directory (renameFile)
import System.IO        (hClose, openTempFile)
import Text.Printf      (printf)

import Data.Function
import Flow
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text           qualified as T
import Data.Text.IO        qualified as TIO
import Options.Applicative qualified as O
import Data.Char (isSpace)

----------------------------------------
-- Types
----------------------------------------

data Location = Location
    { locFilePath   :: FilePath
    , locLineNumber :: Int
    } deriving (Show, Read, Eq)

loc2string :: Location -> String
loc2string Location {..} =
    printf "%s:%d" locFilePath locLineNumber

data Todo = Todo
    { todoId     :: Maybe String
    , todoPrefix :: String
    , todoSuffix :: String
    , todoLoc    :: Maybe Location
    } deriving (Show, Read, Eq)

showTodo :: Todo -> String
showTodo (Todo (Just id') pref suff _) =
    printf "%sTODO: (%s) %s"
        pref id' suff
showTodo (Todo _ pref suff _) =
    printf "%sTODO: %s"
        pref suff

noLocTodo :: String -> String -> String -> Todo
noLocTodo (pure -> id') pref suff =
    Todo id' pref suff Nothing

noIdAndLocTodo :: String -> String -> Todo
noIdAndLocTodo pref suff =
    Todo Nothing pref suff Nothing

addLoc :: FilePath -> Int -> Todo -> Todo
addLoc f l t = t {todoLoc = Location f l &Just}

----------------------------------------
-- Parser
----------------------------------------

type Parser = Parsec Void Text

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

withIdTodoP :: Parser Todo
withIdTodoP = do
    pref <- manyTill anySingle (lookAhead (string "TODO"))
    _    <- string' "todo"
    _    <- space *> char ':' <* space
    id'  <- inParens (manyTill anySingle (lookAhead (char ')')))
    suff <- manyTill anySingle eof
    pure $ noLocTodo id' pref suff

noIdTodoP :: Parser Todo
noIdTodoP = do
    pref <- manyTill anySingle (lookAhead (string "TODO"))
    _    <- string' "todo"
    _    <- space *> char ':' <* space
    suff <- manyTill anySingle eof
    pure $ noIdAndLocTodo pref suff

todoP :: Parser Todo
todoP = try withIdTodoP <|> noIdTodoP

----------------------------------------
-- File IOs
----------------------------------------

infixl 0 ||>
(||>) :: Functor f => f a -> (a -> b) -> f b
a ||> f = f <$> a

extractTodos :: FilePath -> IO [Todo]
extractTodos fname = do
    fname
         |> TIO.readFile
        ||> T.lines
        ||> zip [1..]
        ||> fmap (fmap (parseMaybe todoP))
        ||> mapMaybe \(a, b) -> addLoc fname a <$> b

files2todos :: [FilePath] -> IO [Todo]
files2todos fnames = concat <$> mapM extractTodos fnames

registerTodo :: Todo -> IO Todo
registerTodo todo = do
    time <- getCurrentTime
        <&> formatTime defaultTimeLocale "%Y%md%H%M%S%q"
    pure todo{todoId = time & take 20 & Just}

persistTodo :: Todo -> IO String
persistTodo t = do
    let t' = showTodo t
    let Location f l = fromJust t.todoLoc
    replaceAtLine l f t'
    pure (fromJust t.todoId)

replaceAtLine :: Int -> FilePath -> String -> IO ()
replaceAtLine lnum fname (T.pack -> text) =
    fname
      & TIO.readFile
    >>= T.lines
     .> process
  where
    process ls | lnum <= 0 || lnum > length ls =
        printf
            "[ERROR] replaceAtLine: line number %d is out of bounds."
            lnum
    process ls = do
        let (hd, tl) = ls &splitAt (lnum - 1)
        let ls' = hd ++[ text ]++ drop 1 tl
        let f'  = T.unlines ls'
        (tmpFile, tmpHandle) <- openTempFile "." ".src-todo-temp.txt"
        TIO.hPutStr tmpHandle f'
        hClose tmpHandle
        renameFile tmpFile fname

----------------------------------------
-- Commands
----------------------------------------

data Command
    = Register [FilePath]
    | Show String [FilePath]
    | List [FilePath]
    | ReplaceId String String [FilePath]

files :: O.Parser [FilePath]
files =
    O.metavar "FILES..."
    & O.argument O.str
    & O.some

register :: O.Parser Command
register = Register <$> files

show' :: O.Parser Command
show' =
    Show
    <$> O.argument O.str (O.metavar "ID")
    <*> files

list :: O.Parser Command
list = List <$> files

replaceId :: O.Parser Command
replaceId =
    ReplaceId
    <$> O.argument O.str (O.metavar "OLD_ID")
    <*> O.argument O.str (O.metavar "NEW_ID")
    <*> files

opts :: O.Parser Command
opts =
    O.subparser
    <| ( O.progDesc "Register new todos"
       & O.info      register
       & O.command  "register" )
    <> ( O.progDesc "Show a todo by id"
       & O.info      show'
       & O.command  "show" )
    <> ( O.progDesc "List all todos"
       & O.info      list
       & O.command  "list" )
    <> ( O.progDesc "Replace a todo's id"
       & O.info      replaceId
       & O.command  "replace-id" )

handleCommand :: Command -> IO ()
handleCommand = \case
    Register fnames -> do
        todos <-
            files2todos fnames
            >>= traverse registerTodo
              . filter (todoId .> isNothing)
        ids <- unlines <$> mapM persistTodo todos
        unless (null ids) do
            printf "Registered new todos with these ids:\n%s" ids

    Show id' fnames -> do
        todos <- files2todos fnames
        forM_ todos \t ->
            when (t.todoId == Just id') do
                t & showTodo & putStrLn

    List fnames ->
        files2todos fnames
        >>= mapM_ (showTodo .> putStrLn)

    ReplaceId oldId newId fnames -> do
        todos <- files2todos fnames
        forM_ todos \t ->
            when (t.todoId == Just oldId) do
                let t' = t{todoId = Just newId}
                _ <- t' & persistTodo
                printf "The id %s is replaced with %s" oldId newId
