{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Function      ((&))
import Data.Maybe         (mapMaybe)
import Data.Text          (Text)
import Data.Time          (getCurrentTime)
import Data.Void          (Void)
import System.Environment (getArgs)
import Text.Printf        (printf)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text.IO qualified as TIO
import Data.Text    qualified as T
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import Data.Functor ((<&>))

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
    printf "%sTODO: (20250913111601809444)      %s"
        pref id' suff
showTodo (Todo _ pref suff _) =
    printf "%sTODO: (20250913111601809481)     %s"
        pref suff

noLocTodo :: String -> String -> String -> Todo
noLocTodo (pure -> id') pref suff =
    Todo id' pref suff Nothing

noIdAndLocTodo :: String -> String -> Todo
noIdAndLocTodo pref suff =
    Todo Nothing pref suff Nothing

addLoc :: Location -> Todo -> Todo
addLoc l t = t {todoLoc = Just l}

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

extractTodos :: FilePath -> IO [Todo]
extractTodos fname = do
    lns <-
        fname
         &  TIO.readFile
        <&> zip [1 :: Int ..]
         .  T.lines
    pure []
    -- mapMaybe (parseMaybe todoP) . T.lines <$> TIO.readFile fname

files2todos :: [FilePath] -> IO [Todo]
files2todos fnames = concat <$> mapM extractTodos fnames

registerTodo :: Todo -> IO Todo
registerTodo todo = do
    time <- getCurrentTime
        <&> formatTime defaultTimeLocale "%Y%m%d%H%M%S%q"
    pure todo {todoId = Just time}

-- TODO: (20250913111601810144)  Handle all cases.
handleArgs :: [String] -> IO ()
handleArgs = \case
    "register" : files -> do
        todos <-
            files2todos files
            >>= traverse registerTodo
        mapM_ (putStrLn . showTodo) todos
    _ -> undefined

main :: IO ()
main = getArgs >>= handleArgs

-- TODO: (20250913111717686320) new todo to be registered.
lkjasfjl = 12

-- TODO: (20250913111717686337) another one at the end.
