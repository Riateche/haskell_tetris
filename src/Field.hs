module Field where

import Data.List
import Data.Maybe

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

show_cell value | value == 0 = ".."
                | value == static_value = "██"
                | value == falling_value = "▓▓"
                | value == collision_value = "░░"

show_field :: Field -> String
show_field field = intercalate "\n" $ map (\x -> intercalate "" (map show_cell x)) field

apply_n_times n func = foldr (.) id (replicate n func)

figure_types = ['I', 'J', 'L', 'O', 'T', 'S', 'Z']

generate_original_figure t | t == 'I' = [[1, 1, 1, 1]]
                           | t == 'O' = [[1, 1],
                                         [1, 1]]
                           | t == 'T' = [[1, 1, 1],
                                         [0, 1, 0]]
                           | t == 'J' = [[1, 1, 1],
                                         [0, 0, 1]]
                           | t == 'L' = [[1, 1, 1],
                                         [1, 0, 0]]
                           | t == 'S' = [[0, 1, 1],
                                         [1, 1, 0]]
                           | t == 'Z' = [[1, 1, 0],
                                         [0, 1, 1]]


generate_figure :: Char -> Int -> Field
generate_figure t rotation =
  let figure = generate_original_figure t
      rotated_figure = (apply_n_times rotation rotate_figure) figure
      figure_columns = length $ head rotated_figure
      left = (width - figure_columns) `div` 2
  in expand_figure rotated_figure 0 left

expand_figure figure top left =
  let figure_columns = length $ head figure
      figure_rows = length figure
      right = width - figure_columns - left
      bottom = height - figure_rows - top
      figure_with_columns =
        map (\x -> (replicate left 0) ++ x ++ (replicate right 0)) figure
  in (replicate top (replicate width 0)) ++ figure_with_columns ++ (replicate bottom (replicate width 0))

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

complete_fall_one x | x == falling_value = static_value
complete_fall_one x = x

delete_filled_rows field =
  let f2 = filter (not . all (== static_value)) field
  in  (replicate (height - length f2) (replicate width 0)) ++ f2

complete_fall field = delete_filled_rows $ map (map complete_fall_one) field

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

apply_command field char | char == 'd' =
  if all can_fall_column field
    then map step_fall_column field
    else field

apply_command field char | char == 'a' =
  if all can_fall_column (map reverse field)
    then map (reverse . step_fall_column . reverse) field
    else field

apply_command field _ = field

cut_figure dynamic =
  let cut = filter (all (==0))
  in transpose $ cut $ transpose $ cut $ dynamic

top_offset figure = fromJust $ findIndex (any (>0)) figure

left_offset figure = top_offset $ transpose figure

rotate_figure figure = map reverse $ transpose figure

--rotate_command field =
--  let static = map (map (filter_int static_value)) field
--      dynamic = map (map (filter_int falling_value)) field
--      figure = cut_figure dynamic
--      rotated = map inverse $ transpose figure
