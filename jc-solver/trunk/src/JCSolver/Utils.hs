module JCSolver.Utils where

import Data.List (
    intercalate,
    )

unsafeZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
unsafeZipWith f (x : xs) (y : ys) = f x y : unsafeZipWith f xs ys
unsafeZipWith _ [] [] = []
unsafeZipWith _ _ _ = undefined

unsafeTranspose :: [[a]] -> [[a]]
unsafeTranspose xss
    | all null xss = []
    | otherwise = map head xss : unsafeTranspose (map tail xss)

unsafeZipWithM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
unsafeZipWithM f xs ys = sequence $ unsafeZipWith f xs ys

varyNth :: Monad m => (a -> m a) -> [a] -> Int -> m [a]
varyNth f xs idx = do
    let (xs1, x : xs2) = splitAt idx xs
    x' <- f x
    return $ xs1 ++ x' : xs2

wrap :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
wrap f mx my = do
    x <- mx
    y <- my
    f x y

varIntercalate :: Int -> [a] -> [a] -> [[a]] -> [a]
varIntercalate step xs xs' xss =
    let (xss1, xss2) = splitAt step xss
    in intercalate xs xss1 ++ if null xss2 then [] else xs' ++ varIntercalate step xs xs' xss2
    