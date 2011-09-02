module ReadGraph (file_to_graph, file_to_tikz)
where

{-# OPTIONS_GHC -package parsec #-} -- pragma must come after module lol!

import Data.Char(isSpace)
import Data.List(nub)
import Text.Parsec.String(Parser)
import Text.Parsec(
 (<|>), anyChar, anyToken, char, digit, eof, letter, lookAhead, many,
 many1, manyTill, newline, parse, parseTest, satisfy, space, spaces)

import ISubIso
import FunkyMonadics


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
