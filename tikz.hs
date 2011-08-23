import System.Environment(getArgs)
import ISubIso

-- explicit pattern [_G] to generate cryptic error message on invalid usage
main = getArgs >>= \[_G] -> file_to_tikz _G >>= putStrLn . tail