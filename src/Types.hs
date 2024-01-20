module Types(Bag(..)) where


data Bag = Bag {
    red :: Int,
    blue :: Int,
    green :: Int
} deriving (Eq, Show)