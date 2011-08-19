import Data.Maybe(maybe, fromJust)
import Data.List (sort)

endl = putStr "\n"
main = endl >> print cube >> print cyc6

cube = make_graph v e where
   v = [0,1,2,3,4,5,6,7]
   e = [(0,1), (0,2), (0,4), (1,3), (1,5), (2,3),
        (2,6), (3,7), (4,5), (4,6), (5,7), (6,7)]

cyc6 = make_graph v e where
   v = [0,1,2,3,4,5]
   e = [(0,1), (1,2), (2,3), (3,4), (4,5), (5,0)]


-- Vertex <content> <neighbours> <nonneighbours>
data Vertex = Vertex Int [Vertex] [Vertex]
type Graph  = [Vertex]

instance Show Vertex where
 show (Vertex i xs _) = ' ' : show i ++ ": " ++
  show (map (\(Vertex i _ _) -> i) xs) ++ "\n"

-- creates graph from list of vertices and list of edges
-- the integral identifiers are obligatory
-- we need a sorted list to achieve linear intersection time
make_graph :: [Int] -> [(Int, Int)] -> Graph
make_graph unsorted_vertices unsorted_edges = extract verts    where
 -- sorted vertices
 verts    = sort unsorted_vertices
 -- N_tree dictionary of vertices indexed by Int ID
 vtree    = foldr add_vertex (n_tree Nothing) verts            where
            add_vertex i = update (Just (create i)) i
 -- N_tree of unsorted adjacency lists indexed by vertex ID
 u_adj    = foldr add_edge   (n_tree []     ) unsorted_edges   where
            add_edge (i, j) = modify (j :) i . modify (i :) j
 -- N_tree of sorted adjacency/nonadjacency lists indexed by vertex ID
 etree    = foldr organise   (n_tree ([],[])) verts            where
            organise i = update (neighbours, nonneighbs) i     where
                         neighbours = sort (u_adj ! i)
                         nonneighbs = minus verts neighbours
 create i = Vertex i (extract neighbours) (extract nonneighbs) where
            (neighbours, nonneighbs) = etree ! i
 extract  = map (fromJust . (vtree !))


-- Natural tree: a dictionary with members of [0, 1 ..] as key
data N_tree a = N_tree a (N_tree a) (N_tree a)

-- creates a natural tree where every key is mapped to a default value
n_tree x = let tree = N_tree x tree tree in tree

-- dictionary ! key = value
(!) :: (N_tree a) -> Int -> a
(!) (N_tree x left right) i = case compare i 0 of
 LT -> error "(!): negative key"
 EQ -> x
 GT -> (if odd i then left else right) ! j where j = div i 2

-- modify a single entry of N_tree through a function f
modify :: (a -> a) -> Int -> (N_tree a) -> (N_tree a)
modify f = g where
 g i (N_tree x left right) = let j = div i 2 in case compare i 0 of
  LT ->               error "modify: negative key"
  EQ ->               N_tree (f x)      left       right
  GT -> if odd i then N_tree    x  (g j left)      right  else
                      N_tree    x       left  (g j right)

update :: a -> Int -> (N_tree a) -> (N_tree a)
update x = modify (const x)


-- sorted list intersection
-- similar to the experimental (as of 19 Aug 2011) Data.List.Ordered.isect
isect :: Ord a => [a] -> [a] -> [a]
isect = mergeWith
{- null ys -} (const [])
{- null xs -} (const [])
{- x < y   -} (\xs ys -> isect (tail xs) ys)
{- x = y   -} (\xs ys -> head xs : isect (tail xs) (tail ys))
{- x > y   -} (\xs ys -> isect xs (tail ys))

-- sorted list difference
-- similar to the experimental (as of 19 Aug 2011) Data.List.Ordered.minus
minus :: Ord a => [a] -> [a] -> [a]
minus = mergeWith
{- null ys -} id
{- null xs -} (const [])
{- x < y   -} (\xs ys -> head xs : minus (tail xs) ys)
{- x = y   -} (\xs ys -> minus (tail xs) (tail ys))
{- x > y   -} (\xs ys -> minus xs (tail ys))

-- a common pattern between isect and minus
mergeWith fx fy lt eq gt = recurse where
 recurse xs [] = fx xs
 recurse [] ys = fy ys
 recurse xs ys = case compare (head xs) (head ys) of
           LT -> lt xs ys
           EQ -> eq xs ys
           GT -> gt xs ys
