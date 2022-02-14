module FT where

data FunnyTree a = FunnyNode a [FunnyTree a] deriving (Show,Eq)

treeNodeCount :: FunnyTree a -> Int
treeNodeCount (FunnyNode _ []) = 1
treeNodeCount (FunnyNode _ subTree) = 1 + sum (map treeNodeCount subTree)

treeDepth :: FunnyTree a -> Int
treeDepth (FunnyNode _ []) = 1
treeDepth (FunnyNode _ subTree) = 1 + maximum (map treeDepth subTree)

treeMap :: (a->b) -> FunnyTree a -> FunnyTree b
treeMap f (FunnyNode x []) = FunnyNode (f x) []
treeMap f (FunnyNode x subTree) = FunnyNode (f x) (map (treeMap f) subTree)

treeFold :: (a->b->b) -> b -> FunnyTree a -> b
treeFold _ acc (FunnyNode _ []) = acc
-- treeFold f acc (FunnyNode x subTree) = treeFold f (\_ -> foldr f acc [x]) subTree
