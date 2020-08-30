
module List where

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

(+-+) :: [a] -> [a] -> [a]
(+-+) [] ys = ys
(+-+) (x:xs) ys = x : xs +-+ ys

(+++) :: [a] -> [a] -> [a]
(+++) = flip (foldr (:))