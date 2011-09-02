module ISubIso (
 Graph, Vertex,
 graph, isub, isub_list,
 identifier, neighbourhood
 ) where

import Data.Maybe(fromJust, listToMaybe)
import Data.List(sort)


{-
-- Usage example and preliminary test code

c6 = graph _V _E where
 _V = [0 .. 5]
 _E = [(0,1), (1,2), (2,3), (3,4), (4,5), (5,0)]

cube = graph _V _E where
 _V = [0 .. 7]
 _E = [(0,1), (0,2), (1,3), (2,3), (0,4), (1,5),
       (4,5), (4,6), (5,7), (6,7), (2,6), (3,7)]

main = print (isub c6 cube)
-}


-- Natural tree 'N_tree' is a dictionary with 0, 1 .. as key

data N_tree a = N_tree a (N_tree a) (N_tree a)

-- creates a natural tree where every key is mapped to a default value

n_tree x = let tree = N_tree x tree tree in tree

-- dictionary ! key = value

(!) :: (N_tree a) -> Int -> a
(N_tree x left right) ! i = case compare i 0 of
 LT -> error "(!): negative key"
 EQ -> x
 GT -> (if odd i then left else right) ! (div i 2)

-- modify a single entry of N_tree through a function f

modify :: (a -> a) -> Int -> (N_tree a) -> (N_tree a)
modify f = g where
 g i (N_tree x left right) = let j = div i 2 in case compare i 0 of
  LT -> error "modify: negative key"
  EQ -> N_tree (f x) left right
  GT -> if odd i
   then N_tree x (g j left) right
   else N_tree x left (g j right)

-- replace a single entry with new value x

update :: a -> Int -> (N_tree a) -> (N_tree a)
update x = modify (const x)

-- insert an entry into a sorted list
-- identical to Data.List.Ordered.insertBagBy (data-ordlist-0.2)

insert_by :: (a -> a -> Ordering) -> a -> [a] -> [a]
insert_by cmp = loop where
 loop x [] = [x]
 loop x (y:ys) = case cmp x y of
  GT -> y : loop x ys
  _ -> x : y : ys

-- sorted list intersection
-- identical to Data.List.Ordered.isectBy (data-ordlist-0.2)

isect_by :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
isect_by cmp = loop where
 loop [] _ = []
 loop _ [] = []
 loop (x:xs) (y:ys) = case cmp x y of
  LT -> loop xs (y:ys)
  EQ -> x : loop xs ys
  GT -> loop (x:xs) ys

-- sorted list difference
-- identical to Data.List.Ordered.minusBy (data-ordlist-0.2) 

minus_by :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
minus_by cmp = loop where
 loop [] _ = []
 loop xs [] = xs
 loop (x:xs) (y:ys) = case cmp x y of
  LT -> x : loop xs (y:ys)
  EQ -> loop xs ys
  GT -> loop (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus = minus_by compare

data Vertex = V { identifier :: Int, neighbourhood :: [Vertex] }
type Graph = [Vertex]

instance Show Vertex where
 show (V i xs) = '\n':' ':show i ++ ": " ++ show (map (\(V i _) -> i) xs)

-- |@graph _V _E@ produces a graph whose vertices are @_V@ and edges are @_E@.
-- Edges in @_E@ must be unique and unordered:
-- if @(0, 1)@ is an element of @_E@ then @(1, 0)@ should not be.

graph :: [Int] -> [(Int, Int)] -> Graph
graph unsorted_vertices unsorted_edges = extract verts where
 verts = sort unsorted_vertices
 vtree = foldr add_vertex (n_tree Nothing) verts where
  add_vertex i = update (Just (create i)) i
 etree = foldr add_edge (n_tree []) unsorted_edges where
  add_edge (i, j) = modify (j :) i . modify (i :) j
 extract = map (fromJust . (vtree !))
 create i = V i (extract (sort (etree ! i)))

gcompare :: Vertex -> Vertex -> Ordering
gcompare (V i _) (V j _) = compare i j

ginsert :: Vertex -> [Vertex] -> [Vertex]
ginsert = insert_by gcompare

gisect :: [Vertex] -> [Vertex] -> [Vertex]
gisect = isect_by gcompare

gminus :: [Vertex] -> [Vertex] -> [Vertex]
gminus = minus_by gcompare

-- outputs a boolean list the same length as the first argument
-- indicating the membership of each element of the first list
-- in the second list. expect both lists to be sorted.

membership :: [Vertex] -> [Vertex] -> [Bool]
membership [] _ = []
membership xs [] = map (const False) xs
membership (x:xs) (y:ys) = case gcompare x y of
 LT -> False : membership xs (y:ys)
 EQ -> True : membership xs ys
 GT -> membership (x:xs) ys

-- |@amap@ is the accumulative map.
-- @amap f back front@ is semantically equivalent to, but faster than,
-- @(map f front) ++ back@.

amap :: (a -> b) -> [b] -> [a] -> [b]
amap f = foldr ((:) . f)

-- |@isub_c _H vss@ expects @vss@ to be as long as @_H@. Each
-- element of @vss@ is a list of vertices of a graph @_G@ that could
-- be isomorphic to the element of @_H@ at the same position.
-- Successive activations of @isub_c@ prune the candidate list. The
-- return value is a list of list of vertices of @_G@ isomorphic to
-- @_H@.

isub_c :: Graph -> [[Vertex]] -> [[Vertex]]
isub_c (a:as) xss = loop xss where
 adj_to_a = membership as (neighbourhood a)
 loop ([]:_) = []
 loop [gs] = map (:[]) gs
 loop ((v:vs):vss) = amap (v:) (loop (vs:vss)) (isub_c as new_vss) where
  new_vss = zipWith trim_candidates adj_to_a vss where
   trim_candidates is_neighbour candidates =
    if is_neighbour
    then gisect candidates (neighbourhood v)
    else gminus candidates (ginsert v (neighbourhood v))

-- |@isub_list _H _G@ produces a list of lists of vertices of @_G@
-- that induce subgraphs of @_G@ isomorphic to @_H@.

isub_list :: Graph -> Graph -> [[Vertex]]
isub_list _H _G = isub_c _H (map (const _G) _H)

-- |@isub _H _G@ produces the lexicographically minimum induced
-- subgraph of @_G@ isomorphic to @_H@ if exists.

isub :: Graph -> Graph -> Maybe [Vertex]
isub _H _G = listToMaybe (isub_list _H _G)
