module Main where

import System.IO
import Field

play_figure :: Field -> IO ()
play_figure field | has_collision field = do
  putStrLn (show_field field)
  putStrLn "Game Why No Space Over"


play_figure field = do
  putStrLn "ok1"
  game_iteration field

game_iteration :: Field -> IO ()
game_iteration field | can_fall field = do
  putStrLn "ok2"
  line <- getLine
  let new_field = apply_command (step_fall field) line
  putStrLn (show_field new_field)
  hFlush stdout
  game_iteration new_field

game_iteration field = do
  putStrLn "ok3"
  play_figure(add_figure (complete_fall field) (generate_figure "test"))

--game_iteration :: [[Integer]] -> IO ()
--game_iteration d = do
--  line <- getLine                                     -- line :: String
--  putStrLn ("you said: " ++ line)
--  putStrLn (show_field d)
--  hFlush stdout
--  game_iteration (fst (update_field (d, (is_x line))))


main :: IO ()
main = do
  play_figure(add_figure empty_field (generate_figure "test"))
