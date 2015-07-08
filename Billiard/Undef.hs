-- Модуль для быстрых проверок идей и дурачества
module Undef
(
) where
import Vector2D

arr = [1, 1, 1, 1]

check :: (Num a) => [a] -> a -> [a]
check arr a = map ((+) a) arr

tmp :: [a] -> [a]
tmp arr = res where
  temp = []
  res = arr

asset :: (Eq a) => a -> a -> Bool
asset a b = a == b

main :: IO ()
main = do
    let v = Vector2D 1 1
    let v' = Vector2D 2 2
    print $ v `asset` v'
