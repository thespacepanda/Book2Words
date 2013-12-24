import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.Process (readProcess)
import Data.Char (toLower, isAlpha)
import Data.List (sortBy, group, sort)
import Data.Function (on)
import Control.Arrow((&&&))
import Control.Exception (evaluate)
import qualified Data.Text as T

type Text = T.Text

main :: IO ()
main = getArgs >>= mapM_ makeWordFile

makeWordFile :: FilePath -> IO ()
makeWordFile book = do
  content <- readFile book
  writeFile (takeBaseName book ++ ".words") =<< (writeDefinitions $ parseBook content)

parseBook :: String -> String
parseBook = format . orderWords . getWords

getWords :: String -> [Text]
getWords = map (T.filter isAlpha) . T.words . T.pack . map toLower

orderWords :: [Text] -> [Text]
orderWords = map fst . sortWords . getFrequency

getFrequency :: [Text] -> [(Text, Int)]
getFrequency = map (head &&& length) . group . sort

sortWords :: [(Text, Int)] -> [(Text, Int)]
sortWords = sortBy (flip compare `on` snd)

format :: [Text] -> String
format = T.unpack . T.unlines . map (flip T.snoc '\t')

writeDefinitions :: String -> IO String
writeDefinitions bookWords = do
  let wordList = lines bookWords
  wordDefs <- mapM (\word -> do { xs <- readProcess "dict"
                                        ["-d", "spa-eng", init word] [];
                                  evaluate (length xs);
                                  return xs }) wordList
  let finish = zipWith (++) wordList wordDefs
  return $ unlines finish
