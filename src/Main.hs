module Main where

import System.IO
import Field

put_field field = do
  putStrLn "Field:"
  putStrLn (show_field field)
  hFlush stdout

start_new_figure field = do
 let field2 = add_figure field (generate_figure "test")
 if has_collision field2
   then do
     put_field field2
     putStrLn "Game Why No Space Over"
   else query_command field2

query_command field = do
  put_field field
  putStrLn "Command:"
  hFlush stdout
  line <- getLine
  let new_field = apply_command (field) line
  put_field new_field
  if can_fall new_field
    then query_command (step_fall new_field)
    else do
      let f = complete_fall new_field
      put_field f
      start_new_figure f



main :: IO ()
main = start_new_figure empty_field
