module Main where

import System.IO
import Field
import Control.Monad
import UI.NCurses


type RngGen = Int

rnd_new = 1

rnd_get gen = (gen, gen+1)

random_figure_type value = figure_types !! (value `mod` (length figure_types))


do_render window field color = do
  updateWindow window $ do
    clear
    moveCursor 0 0
    setColor color
    drawString $ show_field field
    drawString "\n\n"
  render

field_iteration field rnd_gen =
  if can_fall field
    then (step_fall field, rnd_gen)
    else let (fig, rnd_gen2) = generate_random_figure rnd_gen
         in (add_figure (complete_fall field) fig, rnd_gen2)


game_iteration window rnd_gen field color = do
  do_render window field color
  if has_collision field
    then do
      updateWindow window $ do
        drawString "\nGame Over!\n"
      render
      getEvent window Nothing
      closeWindow window
    else do
      e <- getEvent window (Just 1000)
      case e of
        Just (EventCharacter char) ->
          case char of
            'q' -> closeWindow window
            's' -> let (field2, rnd_gen2) = field_iteration field rnd_gen
                   in game_iteration window rnd_gen2 field2 color
            _ -> game_iteration window rnd_gen (apply_command field char) color
        _ -> let (field2, rnd_gen2) = field_iteration field rnd_gen
             in game_iteration window rnd_gen2 field2 color

generate_random_figure rnd_gen =
  let (rnd_value1, rnd_gen1) = rnd_get rnd_gen
      (rnd_value2, rnd_gen2) = rnd_get rnd_gen1
      fig = generate_figure (random_figure_type rnd_value1) (rnd_value2 `mod` 4)
  in (fig, rnd_gen2)


main :: IO ()
main = runCurses $ do
  setEcho False
  window <- defaultWindow
  color <- newColorID ColorRed ColorBlack 1
  let (fig, rnd_gen) = generate_random_figure rnd_new
  return ()
  game_iteration window rnd_gen (add_figure empty_field fig) color
