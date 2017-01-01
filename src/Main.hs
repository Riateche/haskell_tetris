module Main where

import System.IO
import Data.List

width = 10
height = 5

init_line = [1] ++ (replicate (width - 1) 0)
init_array = replicate height (init_line)

update_line :: ([Integer], Integer) -> ([Integer], Integer)
update_line (d, v) = do
  let x = last d
  ([v] ++ init d, x)


--update_line d | last d == 1 = init_line
--update_line d = do
--  [0] ++ (take (width - 1) d)

--update_line ([1], False) = ([0], True)
--
--update_line ((x:xs), False) | x == 1 = ([0, 1] ++ (drop 1 (update_line (xs, False))), False)
--                   | x == 0 = do
--                    let (a, b) = update_line (xs, False)
--                    ([0] ++ a, b)
--
--update_line ([], False) = ([], False)
--
--update_line (x, True) = do
--  let (a, b) = update_line (x, False)
--  ([1] ++ (drop 1 a), b)

update_field :: ([[Integer]], Integer) -> ([[Integer]], Integer)
update_field ((x:xs), v) = do
  let (new_x, new_v) = update_line (x, v)
  ([new_x] ++ (fst (update_field (xs, new_v))), new_v)

update_field ([], v) = ([], 0)

is_x s = case s of
  "x" -> 1
  _ -> 0

game_iteration :: [[Integer]] -> IO ()
game_iteration d = do
  line <- getLine                                     -- line :: String
  putStrLn ("you said: " ++ line)
  putStrLn (intercalate "\n" (map show d))
  hFlush stdout
  game_iteration (fst (update_field (d, (is_x line))))


main :: IO ()
main = do
  game_iteration init_array
