import           Control.Monad
import           Data.Monoid
import           Fifteen.Excercises (First')
import           Test.QuickCheck    (quickCheck)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
    quickCheck (monoidLeftIdentity :: First' (Sum Int) -> Bool)
    quickCheck (monoidRightIdentity :: First' (Sum Int) -> Bool)
