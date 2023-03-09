module Utils where

{- | Применение функции только к голове списка, если она есть. -}
headMap :: (a -> a) -> [a] -> [a]
headMap _ []       = []
headMap f (x : xs) = f x : xs
