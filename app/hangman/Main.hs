module Main where

import           Data.Char        (toLower)
import           System.IO        (BufferMode (NoBuffering), hSetBuffering,
                                   stdout)
import           Thirteen.Hangman (freshPuzzle, randomWord', runGame)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
