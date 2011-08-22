module ISubIso (Graph, Vertex, graph, isubiso) where

{-# OPTIONS_GHC -package parsec #-} -- pragma must come after module lol!

import Control.Monad(mplus)
import Data.Maybe(maybe, fromJust)
import Data.List(sort)

import Data.Char(isSpace)
import Text.Parsec.String(Parser)
import Text.Parsec(
 (<|>), anyChar, anyToken, char, digit, eof, letter, lookAhead, many,
  many1, manyTill, newline, parse, parseTest, satisfy, space, spaces)


endl = putStr "\n"
-- main = endl >> print (isubiso _C6 cube) >> print (isubiso cube _C6)

cube = graph _V _E where
 _V = [0,1,2,3,4,5,6,7]
 _E = [(0,1), (0,2), (0,4), (1,3), (1,5), (2,3),
        (2,6), (3,7), (4,5), (4,6), (5,7), (6,7)]

_C6 = graph _V _E where
 _V = [0,1,2,3,4,5]
 _E = [(0,1), (1,2), (2,3), (3,4), (4,5), (5,0)]


-- Natural tree 'N_tree' is a dictionary with 0, 1 .. as key

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
   trim_candidates is_neighbour candidates = if is_neighbour then
    gisect candidates neibours_v else
    gminus candidates nonneibs_v
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
          deriving Show
-- TODO: convert 'deriving Show' of previous line to tikz code generation

-- have to roll my own C programmer's natural number parser

natural :: Parser Int
natural = spaces >> fmap read (many1 digit)

vertex_data :: Parser Data
vertex_data = fmap (\(name, loc, desc) -> Vertex_data name loc desc)
 (bind_triple natural coordinates description) where
 coordinates = spaces >> fmap (++ ")")
  (lookAhead (char '(') >> manyTill anyChar (char ')'))
 description = fmap (f . f) (many anyChar) where
  f = reverse . dropWhile isSpace

edge_data :: Parser Data
edge_data = fmap (\(generator, arguments) -> Edge_data generator arguments)
 (bind_couple (many letter) (many natural))

-- using 'annotator' is as easy as 'tokeniser >>= annotator'!

annotator :: [String] -> Parser [Data]
annotator = mapM parse_string where
 parse_string s = case parse (vertex_data <|> edge_data) "WoW" s of
  Left krank -> fail ("parse error at " ++ show krank) -- TODO: FAIL??!!
  Right rslt -> return rslt

-- the last stage: code generation, or the meaning of words
-- without which, what would government be?

-- TODO: THINK ABOUT HOW TO GET RID OF THE PARSER BOX
-- SURELY WE HAVE NO NEED OF IT IN CODE GENERATION


main = readFile "graphs/triangle" >>= parseTest (tokeniser >>= annotator)
