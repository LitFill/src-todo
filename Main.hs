{-|
Module      : Main
Description : A command-line tool for managing TODO comments in source code.

This module provides a simple yet effective way to find, list, register,
and manage TODO items embedded within text files. It can recursively
search directories, parse TODOs with and without unique IDs, and
persist changes back to the source files.
-}
module Main where

import qualified Todo.Commands

main :: IO ()
main = Todo.Commands.run
