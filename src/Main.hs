module Main where

import System.IO
import Field

put_field field = do
  putStrLn "Field:"
  putStrLn (show_field field)
  hFlush stdout

start_new_figure field = check_new_figure (add_figure field (generate_figure "test"))

check_new_figure field | has_collision field = do
  put_field field
  putStrLn "Game Why No Space Over"
check_new_figure field = query_command field

query_command field = do
  put_field field
  putStrLn "Command:"
  hFlush stdout
  line <- getLine
  let new_field = apply_command (field) line
  put_field new_field
  check_fall new_field

check_fall field | can_fall field = query_command (step_fall field)
check_fall field = do
  let f = complete_fall field
  put_field f
  start_new_figure f


main :: IO ()
main = do
  start_new_figure empty_field
