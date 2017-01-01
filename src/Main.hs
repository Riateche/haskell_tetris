module Main where

import System.IO
import Field
import Control.Monad
import UI.NCurses

apply_action field char = field

do_render window field = do
  updateWindow window $ do
    clear
    moveCursor 0 0
    drawString $ show_field field
  render

field_iteration field =
  if can_fall field
    then step_fall field
    else add_figure (complete_fall field) (generate_figure "test")


game_iteration:: Window -> Field -> Curses ()
game_iteration window field = do
  do_render window field
  if has_collision field
    then closeWindow window
    else do
      e <- getEvent window (Just 300)
      case e of
        Just x ->
          case x of
            EventCharacter char ->
              case char of
                'q' -> closeWindow window
                _ -> game_iteration window $ apply_action field char
        Nothing -> game_iteration window $ field_iteration field


main :: IO ()
main = runCurses $ do
  setEcho False
  window <- defaultWindow
  game_iteration window $ add_figure empty_field (generate_figure "test")













--put_field field = do
--  putStrLn "Field:"
--  putStrLn (show_field field)
--  hFlush stdout
--
--start_new_figure field = do
-- let field2 = add_figure field (generate_figure "test")
-- if has_collision field2
--   then do
--     put_field field2
--     putStrLn "Game Why No Space Over"
--   else query_command field2
--
--query_command field = do
--  put_field field
--  putStrLn "Command:"
--  hFlush stdout
--  line <- getLine
--  let new_field = apply_command (field) line
--  put_field new_field
--  if can_fall new_field
--    then query_command (step_fall new_field)
--    else do
--      let f = complete_fall new_field
--      put_field f
--      start_new_figure f
--
--
--
----main :: IO ()
----main = start_new_figure empty_field
