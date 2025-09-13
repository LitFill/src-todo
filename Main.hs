{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad      (when, unless, forM_)
import Data.Maybe         (fromJust, isNothing, mapMaybe)
import Data.Text          (Text)
import Data.Time          (getCurrentTime)
import Data.Time.Format   (formatTime, defaultTimeLocale)
import Data.Void          (Void)
import System.Directory   (renameFile)
import System.Environment (getArgs)
import System.IO          (openTempFile, hClose)
import Text.Printf        (printf)

import Data.Function
import Data.Functor
import Flow
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text    qualified as T
import Data.Text.IO qualified as TIO

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
addLoc f l t = t {todoLoc = Just (Location f l)}

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
        <&> formatTime defaultTimeLocale "%Y%m%d%H%M%S%q"
    pure todo {todoId = time &take 20 &Just}

persistTodo :: Todo -> IO String
persistTodo t = do
    let t' = showTodo t
    let Location f l = fromJust t.todoLoc
    replaceAtLine l f t'
    pure (fromJust t.todoId)

replaceAtLine :: Int -> FilePath -> String -> IO ()
replaceAtLine l f (T.pack -> t) = do
    ls <- f |> TIO.readFile ||> T.lines
    if l <= 0 || l > length ls
        then printf "[ERROR] replaceAtLine: line number %d is out of bounds." l
        else do
            let
                (hd, tl) = splitAt (l - 1) ls
                ls' = hd ++ [t] ++ drop 1 tl
                f'  = T.unlines ls'
            (tmpFile, tmpHandle) <- openTempFile "." ".src-todo-temp.txt"
            TIO.hPutStr tmpHandle f'
            hClose tmpHandle
            renameFile tmpFile f

handleArgs :: [String] -> IO ()
handleArgs = \case
    "register" : files -> do
        todos <-
            files2todos files
            >>= traverse registerTodo
              . filter (todoId .> isNothing)
        ids <- unlines <$> mapM persistTodo todos
        unless (null ids) do
            printf "Registered new todos with these ids:\n%s" ids

    "show" : id' : files -> do
        todos <- files2todos files
        forM_ todos \t ->
            when (t.todoId == Just id') do
                t &showTodo &putStrLn

    "list" : files ->
        files2todos files
        >>= mapM_ (showTodo .> putStrLn)

    "replace-id" : oldId : newId : files -> do
        todos <- files2todos files
        forM_ todos \t ->
            when (t.todoId == Just oldId) do
                let t' = t {todoId = Just newId}
                _ <- t' &persistTodo
                printf "The id %s is replaced with %s"
                    oldId newId
    _ -> usage

usage :: IO ()
usage = printf
    "Usage: todo [show <id> | register | list | replace <old-id> <new-id>]  <files...>"

main :: IO ()
main = getArgs >>= handleArgs

