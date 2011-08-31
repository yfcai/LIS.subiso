module ISubIso (Graph, Vertex, file_to_graph, file_to_tikz, graph, isubiso,
 bind_couple, identifier) where

{-# OPTIONS_GHC -package parsec #-} -- pragma must come after module lol!

import Control.Monad(mplus)
import Data.Maybe(maybe, fromJust)
import Data.List(nub, sort)

import Data.Char(isSpace)
import Text.Parsec.String(Parser)
import Text.Parsec(
 (<|>), anyChar, anyToken, char, digit, eof, letter, lookAhead, many,
  many1, manyTill, newline, parse, parseTest, satisfy, space, spaces)


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

-- |@isubiso _H _G@ produces the lexicographically minimum induced
-- subgraph of @_G@ isomorphic to @_H@ if exists.

isubiso :: Graph -> Graph -> Maybe [Vertex]
isubiso _H _G = isosub _H (map (const _G) _H) where
 isosub _ ([]:_) = Nothing
 isosub _ (gs:[]) = Just [head gs]
 isosub (a:as) ((v:vs):vss) = let
  adjacency_to_a = membership as (neighbourhood a)
  new_vss = zipWith trim_candidates adjacency_to_a vss where
   neibours_v = neighbourhood v
   nonneibs_v = ginsert v neibours_v
   trim_candidates is_neighbour candidates = if is_neighbour
    then gisect candidates neibours_v
    else gminus candidates nonneibs_v
  in mplus (fmap (v:) (isosub as new_vss)) (isosub (a:as) (vs:vss))

-- my funky monadic shorthands

-- like >>, but preserves first return value instead of second

(=>>) :: Monad m => m a -> m b -> m a
m1 =>> m2 = m1 >>= (m2 >>) . return

-- equivalent to (liftM ++) m1 m2

(++>>) :: Monad m => m String -> m String -> m String
m1 ++>> m2 = m1 >>= \s -> m2 >>= return . (s ++)

-- combine the results of two monads into a tuple!

bind_couple :: Monad m => m a -> m b -> m (a, b)
bind_couple m1 m2 = m1 >>= \x -> m2 >>= \y -> return (x, y)

-- combine the results of three monads into a tuple!

bind_triple :: Monad m => m a -> m b -> m c -> m (a, b, c)
bind_triple m1 m2 m3 = m1 >>= \x->m2 >>= \y->m3 >>= \z->return (x,y,z)

-- memory wipe: equivalent to m1 >> return ()

forget :: Monad m => m a -> m ()
forget m1 = m1 >> return ()

-- read graph from file
-- generate graph object and tikzpicture

tokeniser :: Parser [String]
tokeniser = meaningless >> many token where
 token            = logical_line =>> meaningless
 logical_line     = starting_line ++>> subsequent_lines
 starting_line    = lookAhead (satisfy (not . isSpace)) >> line
 subsequent_lines = fmap concat (many subsequent_line)
 subsequent_line  = lookAhead space >> line
 meaningless      = many (comment <|> empty_line)
 comment          = forget (char '#' >> line)
 empty_line       = forget newline
 line             = manyTill anyChar eol
 eol              = empty_line <|> eof

-- a token describes either a vertex or an edge

data Data = Vertex_data {tikz_name :: Int, tikz_loc, tikz_desc :: String}
          | Edge_data {edge_generator :: String, edge_arguments :: [Int]}

-- have to roll my own C programmer's natural number parser

natural :: Parser Int
natural = fmap read (many1 digit) =>> spaces

vertex_data :: Parser Data
vertex_data = fmap (\(name, loc, desc) -> Vertex_data name loc desc)
 (bind_triple natural coordinates description) where
 coordinates = fmap (++ ")")
  (lookAhead (char '(') >> manyTill anyChar (char ')')) =>> spaces
 description = fmap (f . f) (many anyChar) where
  f = reverse . dropWhile isSpace

edge_data :: Parser Data
edge_data = fmap (\(generator, arguments) -> Edge_data generator arguments)
 (bind_couple (many1 letter =>> spaces) (many natural))

-- using 'annotator' is as easy as 'tokeniser >>= annotator'!
-- TODO: de-uglify me!

annotator :: [String] -> Parser [Data]
annotator = mapM parse_string where
 parse_string s = case parse (vertex_data <|> edge_data) "WoW" s of
  Left krank -> fail ("parse error at " ++ show krank) -- TODO: FAIL??!!
  Right rslt -> return rslt

-- the last stage: code generation, or the meaning of words
-- without which, what would government be?

-- extract tikz code from vertex/edge data

instance Show Data where
 show graph_data = let
  ref name = "(" ++ show name ++ ")"
  draw sth = "\n\\draw" ++ sth ++ ";"
  walk ns = if null ns then []
   else ref (head ns) ++ ( (tail ns) >>= ("--" ++) . ref)
  edge_to_tikz (x, y) = ref x ++ "--" ++ ref y
  in
  case graph_data of
  (Vertex_data name loc desc) ->
   "\n\\node at" ++ loc ++ ref name ++ "{$" ++ desc ++ "$};"
  (Edge_data generator args) ->
   case generator of
   -- walk and cycle has shorthands in tikz
   -- other edges are printed as is
   "c" -> {-cycle  -} draw (walk args ++ "--" ++ (ref . head) args)
   "w" -> {-walk   -} draw (walk args)
   gen -> {-default-} draw (concatMap edge_to_tikz (gen_edges gen args))

-- let the generator generate edges from arguments :-O

gen_edges :: String -> [Int] -> [(Int, Int)]
gen_edges generator args = let
 make_pair [] = []
 make_pair (x:y:zs) = (x, y) : make_pair zs
 walk = zip args (tail args)
 in case generator of
 "c" -> {-cycle  -} (last args, head args) : walk
 "e" -> {-edges  -} make_pair args
 "k" -> {-clique -} [(u, v) | u <- args, v <- args, u < v]
 "w" -> {-walk   -} walk

-- separate a list of vertices and edges from a list of vertex/edge data

read_graph :: [Data] -> ( [Int], [(Int, Int)] )
read_graph = let
 rough_read [] = ([], [])
 rough_read (datum : _data) = let
  (_V, _E) = rough_read _data
  in case datum of
  Vertex_data v _ _ -> (v : _V, _E)
  Edge_data generator args -> (_V, gen_edges generator args ++ _E)
 nub2 (xs, ys) = (nub xs, nub ys)
 in nub2 . rough_read

-- helpers to the two file_to_* functions

parse_it :: Monad m => String -> Parser a -> String -> m a
parse_it file_name parser input = case parse parser file_name input of
 Left er -> fail ("error at " ++ show er)
 Right x -> return x

file_to :: ([Data] -> a) -> String -> IO a
file_to conversion file_name = readFile file_name
 >>= parse_it file_name (fmap conversion (tokeniser >>= annotator))

-- read file, decode tikz and construct graph object

file_to_graph :: String -> IO Graph
file_to_graph = file_to (uncurry graph . read_graph)

-- read file and piece together some tikz code

file_to_tikz :: String -> IO String
file_to_tikz = file_to (concatMap show)
