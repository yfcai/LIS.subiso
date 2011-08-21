import Control.Monad(mplus)
import Data.Maybe(maybe, fromJust)
import Data.List(sort)


endl = putStr "\n"
main = endl >> print (subiso _C6 cube) >> print (subiso cube _C6)

cube = graph _V _E where
 _V = [0,1,2,3,4,5,6,7]
 _E = [(0,1), (0,2), (0,4), (1,3), (1,5), (2,3),
        (2,6), (3,7), (4,5), (4,6), (5,7), (6,7)]

_C6 = graph _V _E where
 _V = [0,1,2,3,4,5]
 _E = [(0,1), (1,2), (2,3), (3,4), (4,5), (5,0)]


-- Natural tree: a dictionary with members of [0, 1 ..] as key

data N_tree a = N_tree a (N_tree a) (N_tree a)

-- creates a natural tree where every key is mapped to a default value

n_tree x = let tree = N_tree x tree tree in tree

-- dictionary ! key = value

(!) :: (N_tree a) -> Int -> a
(N_tree x left right) ! i = case compare i 0 of
 LT -> error "(!): negative key"
 EQ -> x
 GT -> (if odd i then left else right) ! j where j = div i 2

-- modify a single entry of N_tree through a function f

modify :: (a -> a) -> Int -> (N_tree a) -> (N_tree a)
modify f = g where
 g i (N_tree x left right) = let j = div i 2 in case compare i 0 of
  LT -> error "modify: negative key"
  EQ -> N_tree (f x) left right
  GT -> if odd i then
   N_tree x (g j left) right else
   N_tree x left (g j right)

-- replace a single entry with new value x

update :: a -> Int -> (N_tree a) -> (N_tree a)
update x = modify (const x)

-- insert an entry into a sorted list

insert_by :: (a -> a -> Ordering) -> a -> [a] -> [a]
insert_by cmp = loop where
 loop x [] = [x]
 loop x (y:ys) = case cmp x y of
  LT -> x : y : ys
  _ -> y : loop x ys

-- sorted list intersection
-- identical to Data.List.Ordered.isectBy (19 Aug 2011)

isect_by :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
isect_by cmp = loop where
 loop [] _ = []
 loop _ [] = []
 loop (x:xs) (y:ys) = case cmp x y of
  LT -> loop xs (y:ys)
  EQ -> x : loop xs ys
  GT -> loop (x:xs) ys

-- sorted list difference
-- identical to Data.List.Ordered.minusBy (19 Aug 2011) 

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

-- V <int ID> <neighbours>

data Vertex = V Int [Vertex]
type Graph = [Vertex]

instance Show Vertex where
 show (V i xs) = '\n':' ':show i ++ ": " ++ show (map (\(V i _) -> i) xs)

graph :: [Int] -> [(Int, Int)] -> Graph
graph unsorted_vertices unsorted_edges = extract verts where
 verts = sort unsorted_vertices
 vtree = foldr add_vertex (n_tree Nothing) verts where
  add_vertex i = update (Just (create i)) i
 etree = foldr add_edge (n_tree []) unsorted_edges where
  add_edge (i, j) = modify (j :) i . modify (i :) j
 extract = map (fromJust . (vtree !))
 create i = V i (extract (sort (etree ! i)))

neighbourhood :: Vertex -> [Vertex]
neighbourhood (V _ vs) = vs

identifier :: Vertex -> Int
identifier (V i _) = i

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

-- outputs an induced subgraph of _G isomorphic to _H if exists

subiso :: Graph -> Graph -> Maybe [Vertex]
subiso _H _G = isosub _H (map (const _G) _H) where
 isosub _ ([]:_) = Nothing
 isosub _ (gs:[]) = Just [head gs]
 isosub (a:as) ((v:vs):vss) = let
  adjacency_to_a = membership as (neighbourhood a)
  new_vss = zipWith trim_candidates adjacency_to_a vss where
   neibours_v = neighbourhood v
   nonneibs_v = ginsert v neibours_v
   trim_candidates is_neighbour candidates = if is_neighbour then
    gisect candidates neibours_v else
    gminus candidates nonneibs_v
  in mplus (isosub as new_vss >>= (Just . (v:))) (isosub (a:as) (vs:vss))
