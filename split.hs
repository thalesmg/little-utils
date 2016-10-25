module Split where

import Control.Applicative
import Data.List
import System.Environment
import System.IO

{-
Split a list of objects into n lists.
-}
split :: Int -- ^Number of resulting lists
      -> [a] -- ^Original list of elements
      -> [[a]]
split n lst = [[e | (e, i) <- zip lst [0..], i `mod` n == j] | j <- [0..n-1]]


main :: IO ()
main = do
  [n, file] <- getArgs
  when n <= 0 $ error "The number of resulting files must be greater than 1."
  content <- lines <$> readFile file
  let lstlst = split (read n) content
      names = zipWith (\s num -> s ++ (show num)) (repeat file) [1..]
  mapM_ (\(file', ls) -> writeFile file' (unlines ls)) (zip names lstlst)
