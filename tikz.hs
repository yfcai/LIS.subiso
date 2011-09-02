import System.Environment(getArgs)
import ReadGraph(file_to_tikz)

-- explicit pattern [_G] to generate cryptic error message on invalid usage
main = getArgs >>= \[_G] -> file_to_tikz _G >>= putStrLn . tail