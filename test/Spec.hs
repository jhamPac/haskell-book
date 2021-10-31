import qualified Data.Map        as M
import           Fourteen.Morse  (Morse, charToMorse, letterToMorse,
                                  morseToChar)
import           Test.QuickCheck (Gen, Property, elements, forAll, quickCheck)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_MorseAndBackAgain :: Property
prop_MorseAndBackAgain =
    forAll charGen
        (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_MorseAndBackAgain
