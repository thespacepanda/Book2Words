import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Data.Char (toLower)
import Data.List (nub, sortBy, intersperse)
import Data.Function (on)

main :: IO ()
main = getArgs >>= mapM_ (\arg -> makeWordFile arg)

makeWordFile :: FilePath -> IO ()
makeWordFile book = do
  content <- readFile book
  writeFile (takeBaseName book ++ ".words") (parseBook content)

parseBook :: String -> String
parseBook = prettyPrint . orderWords . getWords

getWords :: String -> [String]
getWords book = filter notPunctuated $ words $ map toLower book

notPunctuated :: String -> Bool
notPunctuated = all (`notElem` "¿¡—«»[]{}*.?!,(:-;’\'\"\")\"\"'")

orderWords :: [String] -> [String]
orderWords = discardCount . sortWords . getFrequency

getFrequency :: [String] -> [(String, Int)]
getFrequency wordList = [ (word, length $ filter (== word) wordList)
                     | word <- nub wordList ]

sortWords :: [(String, Int)] -> [(String, Int)]
sortWords = reverse . sortBy (compare `on` snd)

discardCount :: [(String, Int)] -> [String]
discardCount = map fst

prettyPrint :: [String] -> String
prettyPrint = concat . intersperse "\n"
