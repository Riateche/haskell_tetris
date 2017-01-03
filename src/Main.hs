module Main where

import System.IO
import Field
import Control.Monad
import UI.NCurses

data Game = Game Window Int [[Integer]] ColorID

get_field (Game window rnd_gen field color) = field
get_color (Game window rnd_gen field color) = color
get_rand (Game window rnd_gen field color) = rnd_gen
get_window (Game window rnd_gen field color) = window

apply_cmd (Game window rnd_gen field color) char = Game window rnd_gen (apply_command field char) color

next_rand (Game window rnd_gen field color) = Game window (rnd_gen + 1) field color
insert_field (Game window rnd_gen old_field color) field = Game window rnd_gen field color

rnd_new = 0

do_render window field color = do
  updateWindow window $ do
    clear
    moveCursor 0 0
    setColor color
    drawString $ show_field field
    drawString "\n\n"
  render

field_iteration game =
  if can_fall $ get_field game
    then insert_field game $ step_fall $ get_field game
    else let fig = generate_random_figure $ get_rand game
         in next_rand $ insert_field game $ add_figure (complete_fall $ get_field game) fig


game_iteration game_old = do
  let game = next_rand game_old
  do_render (get_window game) (get_field game) (get_color game)
  if has_collision (get_field game)
    then do
      let window = get_window game
      updateWindow window $ do
        drawString "\nGame Over!\n"
      render
      getEvent window Nothing
      closeWindow window
    else do
      e <- getEvent (get_window game) (Just 1000)
      case e of
        Just (EventCharacter char) ->
          case char of
            'q' -> closeWindow $ get_window game
            's' -> game_iteration $ field_iteration game
            _ -> game_iteration $ apply_cmd game char
        _ -> game_iteration $ field_iteration game

random_figure_type value = figure_types !! (value `mod` (length figure_types))
generate_random_figure rnd = generate_figure (random_figure_type rnd) ((rnd + 1) `mod` 4)


main :: IO ()
main = runCurses $ do
  setEcho False
  window <- defaultWindow
  color <- newColorID ColorRed ColorBlack 1
  let fig = generate_random_figure rnd_new
      game = Game window (rnd_new + 1) (add_figure empty_field fig) color
  game_iteration game
