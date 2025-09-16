module Todo.Parser (todoP) where

import qualified Control.Applicative
import qualified Data.Attoparsec.Text
import qualified Data.Text

import Todo.Types

----------------------------------------
--- * Parser
----------------------------------------

-- | Parser for a 'Todo' with an ID.
--
-- >>> Data.Attoparsec.Text.parseOnly withIdTodoP (Data.Text.pack "  // TODO: (#1234) fix this")
-- Right (Todo {todoId = Just "#1234", todoPrefix = "  // ", todoSuffix = "fix this", todoLoc = Nothing})
withIdTodoP :: Data.Attoparsec.Text.Parser Todo
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

-- | Parser for a 'Todo' without an ID.
--
-- >>> Data.Attoparsec.Text.parseOnly noIdTodoP (Data.Text.pack "  // TODO: fix this")
-- Right (Todo {todoId = Nothing, todoPrefix = "  // ", todoSuffix = "fix this", todoLoc = Nothing})
noIdTodoP :: Data.Attoparsec.Text.Parser Todo
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

-- | The main parser that tries to parse a 'Todo' with an ID first,
-- and if that fails, tries to parse a 'Todo' without an ID.
--
-- >>> Data.Attoparsec.Text.parseOnly todoP (Data.Text.pack "  // TODO: (#1234) fix this")
-- Right (Todo {todoId = Just "#1234", todoPrefix = "  // ", todoSuffix = "fix this", todoLoc = Nothing})
--
-- >>> Data.Attoparsec.Text.parseOnly todoP (Data.Text.pack "  // TODO: fix this")
-- Right (Todo {todoId = Nothing, todoPrefix = "  // ", todoSuffix = "fix this", todoLoc = Nothing})
todoP :: Data.Attoparsec.Text.Parser Todo
todoP = withIdTodoP Control.Applicative.<|> noIdTodoP

-- | Parses prefix up to (case-insensitive) TODO, return (prefix, found?)
--
-- >>> Data.Attoparsec.Text.parseOnly parsePrefixCI (Data.Text.pack "  // TODO: fix this")
-- Right ("  // ",True)
parsePrefixCI :: Data.Attoparsec.Text.Parser (String, Bool)
parsePrefixCI = do
    prefix <- Data.Attoparsec.Text.takeWhile1 isNotT Control.Applicative.<|> pure (Data.Text.pack "")
    rest   <- Data.Attoparsec.Text.take 4
    pure $ if Data.Text.toCaseFold rest == Data.Text.pack "todo"
        then (Data.Text.unpack prefix          , True)
        else (Data.Text.unpack (prefix <> rest), False)
  where
    isNotT c = c /= 'T' && c /= 't'
