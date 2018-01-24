_bubble_search :: [Int] -> Int -> Int -> Int
_bubble_search x value index
  | null x          = -1
  | head x == value = index
  | otherwise       = _bubble_search (tail x) value $ index + 1

bubble_search :: [Int] -> Int -> Int
bubble_search x value
  | null x    = -1
  | otherwise = _bubble_search x value 0


_binary_search :: [Int] -> Int -> Int -> Int -> Int
_binary_search x value low high
  | high < low     = -1
  | x!!mid > value = _binary_search x value low $ mid - 1
  | x!!mid < value = _binary_search x value (mid + 1) high
  | otherwise      = mid
  where
    mid = low + (div (high - low) 2)

binary_search :: [Int] -> Int -> Int
binary_search x value
  | null x    = -1
  | otherwise = _binary_search x value 0 $ length x
