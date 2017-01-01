module Field where

import Data.List

type Field = [[Integer]]

width = 10
height = 6

empty_value = 0
static_value = 1
falling_value = 2
collision_value = 9

add_figure_item x 1 | x == static_value = collision_value
add_figure_item _ 1 = falling_value
add_figure_item x _ = x


empty_field = replicate height (replicate width 0)
show_field d = intercalate "\n" (map show d)

generate_figure :: String -> Field
generate_figure name =
    [[0,0,0,0,1,1,0,0,0,0],
     [0,0,0,0,1,0,0,0,0,0],
     [0,0,0,0,1,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0]]

add_figure :: Field -> Field -> Field
add_figure x y = do
  zipWith (zipWith add_figure_item) x y

has_collision field = elem collision_value (intercalate [] field)

can_fall field = True

step_fall field = field
apply_command field line = field
complete_fall field = field