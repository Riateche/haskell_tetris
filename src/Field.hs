module Field where

import Data.List

type Field = [[Integer]]

width = 10
height = 20

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
  let figure = [[1, 1],[1, 0],[1, 0]]
      figure_columns = 2
      figure_rows = 3
      columns_to_left = (width - figure_columns) `div` 2
      columns_to_right = width - figure_columns - columns_to_left
      figure_with_columns =
        map (\x -> (replicate columns_to_left 0) ++ x ++ (replicate columns_to_right 0)) figure
  in figure_with_columns ++ (replicate (height - figure_rows) (replicate width 0))


add_figure :: Field -> Field -> Field
add_figure x y = zipWith (zipWith add_figure_item) x y

has_collision field = elem collision_value (intercalate [] field)

can_fall_column :: [Integer] -> Bool
can_fall_column col | last col == falling_value = False
can_fall_column col =
  let (static, dynamic) = separate_column col
  in all (\(x, y) -> x * y == 0) (zip static dynamic)

can_fall field = all can_fall_column (transpose field)

step_fall field = transpose (map step_fall_column (transpose field))
apply_command field line = field

complete_fall_one x | x == falling_value = static_value
complete_fall_one x = x

complete_fall field = map (map complete_fall_one) field

step_fall_column :: [Integer] -> [Integer]

filter_int v x | x == v = x
filter_int v x = 0

step_fall_column col =
  let (static, dynamic) = separate_column col
  in zipWith (+) static dynamic

separate_column col =
  let static = map (filter_int static_value) col
      old_dynamic = map (filter_int falling_value) col
      dynamic = 0:(init old_dynamic)
      in (static, dynamic)

