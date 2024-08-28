import Control.Monad
import Control.Monad.Extra
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)


col :: [[a]] -> Int -> [a]
col s x = map (!! x) s

row :: [[a]] -> Int -> [a]
row s y = s !! y

reg :: [[a]] -> Int -> Int -> [a]
reg s x y = [s !! (yReg + y0) !! (xReg + x0)| x0 <- [0..2], y0 <- [0..2]]
    where xReg = x - x `mod` 3
          yReg = y - y `mod` 3

toSudoku :: String -> [[[Int]]]
toSudoku = map (map (filter (0 /=) . singleton . read) . words) . lines

fromSudoku :: [[[Int]]] -> String
fromSudoku = unlines . map (unwords . map (show . head))

events :: [[[Int]]] -> [[[Int]]]
events s = [[aux i j | i <- [0..8]] | j <- [0..8]]
    where aux x y
            | length e == 1 = e
            | otherwise = [1..9] \\ concat (col s x ++ row s y ++ reg s x y)
            where e = s !! y !! x

solve :: [[[Int]]] -> [[[Int]]]
solve s = [[aux i j | i <- [0..8]] | j <- [0..8]]
    where aux x y
            | length e == 1 = e
            | otherwise = as `union` bs `union` cs
            where e = s !! y !! x
                  as = [1..9] \\ concat (delete e $ col s x)
                  bs = [1..9] \\ concat (delete e $ row s y)
                  cs = [1..9] \\ concat (delete e $ reg s x y)

checkFileFormat :: String -> Bool
checkFileFormat s = width s || height s
    where width  = any ((9 /=) . length . words) . lines
          height = (9 /=) . length . lines

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "No file specified"
        exitFailure

    filesExist <- traverse doesFileExist args
    whenJust (findIndex not filesExist) $ \i -> do
        hPutStrLn stderr ("Couldn't find file: " ++ (args !! i))
        exitFailure

    files <- traverse readFile args
    when (any checkFileFormat files) $ do
        hPutStrLn stderr "Incorrect file format"
        exitFailure

    mapM_ (putStrLn . fromSudoku . (!! 50) . iterate (solve . events) . toSudoku) files
