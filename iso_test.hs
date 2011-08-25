import System.Environment(getArgs)
import Data.List(intersperse)
import ISubIso

main = getArgs
 >>= \[_H, _G] -> bind_couple (file_to_graph _H) (file_to_graph _G)
 >>= putStrLn . present _H _G . uncurry isubiso
 where
 present _H _G x = case x of
  Nothing -> _H ++ " is not an induced subgraph of " ++ _G ++ "."
  Just _S -> "These vertices of " ++ _G
   ++ " induce a subgraph isomorphic to " ++ _H
   ++ ":\n{" ++ concat (intersperse ", " (map (show . identifier) _S)) ++ "}"