module Fifteen.Excercises where

import           Test.QuickCheck (Arbitrary (arbitrary), oneof)

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only a)      = Only a
    (Only a) <> Nada      = Only a
    (Only a) <> (Only a') = Only (a <> a')
    Nada <> Nada          = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

-- 15.12
newtype First' a = First' { getFirst :: Optional a } deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where
    (First' (Only a)) <> _ = First' (Only a)
    _ <> (First' (Only b)) = First' (Only b)
    _ <> _                 = First' Nada


instance Monoid a => Monoid (First' a) where
    mempty = First' Nada
    mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        oneof [pure $ First' Nada, pure $ First' (Only a)]
