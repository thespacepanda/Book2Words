import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Data.Char (toLower)
import Data.List (nub, sortBy, intersperse, group, sort)
import Data.Function (on)

main :: IO ()
main = getArgs >>= mapM_ makeWordFile

makeWordFile :: FilePath -> IO ()
makeWordFile book = do
  content <- readFile book
  writeFile (takeBaseName book ++ ".words") (parseBook content)

parseBook :: String -> String
parseBook = unlines . orderWords . getWords

getWords :: String -> [String]
getWords = filter notPunctuated . words . map toLower

notPunctuated :: String -> Bool
notPunctuated = all (`notElem` "¿¡—«»[]{}*.?!,(:-;’\'\"\")\"\"'")

orderWords :: [String] -> [String]
orderWords = map fst . sortWords . getFrequency

getFrequency :: [String] -> [(String, Int)]
getFrequency list = map (\x -> (head x, length x) ) grouped
  where grouped = group . sort $ list

sortWords :: [(String, Int)] -> [(String, Int)]
sortWords = sortBy (flip compare `on` snd)
