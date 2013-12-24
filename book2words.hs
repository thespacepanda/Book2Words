import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Data.Char (toLower, isAlpha)
import Data.List (sortBy, group, sort)
import Data.Function (on)
import Control.Arrow((&&&))
import qualified Data.Text as T

type Text = T.Text

main :: IO ()
main = getArgs >>= mapM_ makeWordFile

makeWordFile :: FilePath -> IO ()
makeWordFile book = do
  content <- readFile book
  writeFile (takeBaseName book ++ ".words") (parseBook content)

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
