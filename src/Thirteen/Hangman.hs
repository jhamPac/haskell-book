module Thirteen.Hangman where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (fromMaybe, isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

newtype WordList =
    WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    pure $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
    (WordList a) <- allWords
    pure $ WordList (filter gameLength a)

    -- filter gameLength <$> allWords
    where
        gameLength w =
            let l = length (w :: String)
            in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    pure $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
    Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discoverd guessed) = intersperse ' ' $ fmap renderPuzzleChar discoverd ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar cs) c =
    Puzzle word newFilledInSoFar (c : cs)
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar

        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            pure puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word accordingly"
            pure (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            pure (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
    if length guessed > 5
    then do putStrLn "Game Over!"
            putStrLn $ "The word was: " ++ word
            exitSuccess
    else pure ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filled _) =
    if all isJust filled
    then do putStrLn "You win, good job!"
            exitSuccess
    else pure ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"
