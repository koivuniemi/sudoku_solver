{- Copyright (c) 2024 Koivuniemi
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 -
 - The above copyright notice and this permission notice shall be included
 - in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 - IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 - CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 - TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 - SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import Text.Read


col :: [[a]] -> Int -> [a]
col s x = map (!! x) s

row :: [[a]] -> Int -> [a]
row s y = s !! y

reg :: [[a]] -> Int -> Int -> [a]
reg s x y = [s !! (yReg + y0) !! (xReg + x0)| x0 <- [0..2], y0 <- [0..2]]
    where xReg = x - x `mod` 3
          yReg = y - y `mod` 3

toSudoku :: String -> [[[Int]]]
toSudoku = map (map (maybe [] singleton . readMaybe) . words) . lines

fromSudoku :: [[[Int]]] -> String
fromSudoku = unlines . map (unwords . map f)
    where f [] = "."
          f e  = show $ head e

events :: [[[Int]]] -> [[[Int]]]
events s = [[f i j | i <- [0..8]] | j <- [0..8]]
    where f x y
            | length e == 1 = e
            | otherwise = [1..9] \\ concat (col s x ++ row s y ++ reg s x y)
            where e = s !! y !! x

solve :: [[[Int]]] -> [[[Int]]]
solve s = [[f i j | i <- [0..8]] | j <- [0..8]]
    where f x y
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

    let fileName = head args
    fileExists <- doesFileExist fileName
    unless fileExists $ do
        hPutStrLn stderr $ "File does not exist: " ++ fileName
        exitFailure

    file <- readFile fileName
    when (checkFileFormat file) $ do
        hPutStrLn stderr "Incorrect file format"
        exitFailure

    putStrLn $ fromSudoku $ (!! 50) $ iterate (solve . events) $ toSudoku file
