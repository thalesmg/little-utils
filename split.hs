import Control.Applicative
import Data.List
import System.Environment
import System.IO

splitar :: Int -> [a] -> [[a]]
splitar n lst = [[e | (e, i) <- zip lst [0..], i `mod` n == j] | j <- [0..n-1]]


main :: IO ()
main = do
  [n, arq] <- getArgs
  conteudo <- lines <$> readFile arq
  let lstlst = splitar (read n) conteudo
      nomes = zipWith (\s num -> s ++ (show num)) (repeat arq) [1..]
  mapM_ (\(arq', ls) -> writeFile arq' (unlines ls)) (zip nomes lstlst)
