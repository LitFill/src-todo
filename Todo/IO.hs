{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Todo.IO (
    files2todos,
    registerTodo,
    persistTodo,
    ) where

import qualified Control.Exception
import qualified Data.Attoparsec.Text
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.UUID
import qualified Data.UUID.V4
import qualified Flow
import qualified System.Directory
import qualified System.Directory.Tree
import qualified System.IO
import qualified Text.Printf

import Todo.Parser
import Todo.Types

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

