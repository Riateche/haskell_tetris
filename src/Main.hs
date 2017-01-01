module Main where

import System.IO
import Field
import Control.Monad
import UI.NCurses

import System.Random

sample x = return.(x!!)=<<randomRIO(0,length x-1)

do_render window field = do
  updateWindow window $ do
    clear
    moveCursor 0 0
    drawString $ show_field field
  render

field_iteration field =
  if can_fall field
    then step_fall field
    else add_figure (complete_fall field) generate_random_figure


game_iteration:: Window -> Field -> Curses ()
game_iteration window field = do
  do_render window field
  if has_collision field
    then do
      updateWindow window $ do
        drawString "\nGame Over!\n"
      render
      getEvent window Nothing
      closeWindow window
    else do
      e <- getEvent window (Just 100)
      case e of
        Just (EventCharacter char) ->
          case char of
            'q' -> closeWindow window
            's' -> game_iteration window $ field_iteration field
            _ -> game_iteration window $ apply_command field char
        _ -> game_iteration window $ field_iteration field

generate_random_figure = do
  generate_figure 'I' 2


main :: IO ()
main = runCurses $ do
  setEcho False
  window <- defaultWindow
  game_iteration window $ add_figure empty_field generate_random_figure
