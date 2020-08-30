
module BinaryTree where

-- import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Node l r) = "<" ++ show l ++ "|" ++ show r ++ ">"

numberOfLeaves :: Tree a -> Int
numberOfLeaves (Leaf _)   = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf x) i = (Leaf (i, x), i+1)
relabel (Node l r) i = 
    let (l', i1) = relabel l i
        (r', i2) = relabel r i1
    in
        (Node l' r', i2)

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = 
    \i -> let (n, i') = f i 
    in g n i'

pure' :: a -> WithCounter a
pure' x = \i -> (x, i)

relabel' :: Tree a -> WithCounter (Tree (Int, a))
relabel' (Leaf x) = \i -> (Leaf (i, x), i+1)
relabel' (Node l r) = relabel l `next` \l' ->
                      relabel r `next` \r' ->
                      pure' (Node l' r')

type State s a = s -> (a, s) 

nextS :: State Int a -> (a -> State Int b) -> State Int b
f `nextS` g = 
    \i -> let (n, i') = f i 
    in g n i'

pureS :: a -> State Int a
pureS x = \i -> (x, i)

relabelS :: Tree a -> State Int (Tree (Int, a))
relabelS (Leaf x) = \i -> (Leaf (i, x), i+1)
relabelS (Node l r) = relabel l `next` \l' ->
                      relabel r `next` \r' ->
                      pure' (Node l' r')

--relabel :: Tree a -> WithCounter (Tree a)

















-- numberOfLeaves :: Tree a -> Int
-- numberOfLeaves (Leaf _)   = 1
-- numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

-- relabel :: Tree a -> Int -> (Tree (a, Int), Int)
-- relabel (Leaf a) i   = (Leaf (a, i), i + 1)
-- relabel (Node l r) i =
--     let
--         (lv, i1) = relabel l i
--         (rv, i2) = relabel r i1
--     in
--         (Node lv rv, i2)

-- type WithCounter a = Int -> (a, Int)
-- type State s a = s -> (a, s)

-- next :: State Int a -> (a -> State Int b) -> State Int b
-- f `next` g = \i -> let (lv, i1) = f i in g lv i1

-- pureWithCounter :: a -> State Int a
-- pureWithCounter x = \i -> (x, i)

-- relabelWithCounter :: Tree a -> State Int (Tree (Int, a))
-- relabelWithCounter (Leaf x) = \i -> (Leaf (i, x), i + 1)
-- relabelWithCounter (Node l r) = relabelWithCounter l `next` \l' ->
--                                 relabelWithCounter r `next` \r' ->
--                                 pureWithCounter (Node l' r')

-- relabelWithCounter (Node l r) = \i ->
--     let
--         (lv, i1) = relabelWithCounter l i
--         (rv, i2) = relabelWithCounter r i1
--     in
--         (Node lv rv, i2)

-- t = Node (Leaf 3) (Leaf 5)
