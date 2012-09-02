module JCSolver.Solve (
    ForkFunction,
    TransformFunction,
    Completable (..),
    Syncable (..),
    slLoop,
    slSmartLoop,
    slForkAndSyncAll,
    slForkAndSmartSync,
    slAllSolutions,
    ) where

import Data.Maybe (
    mapMaybe,
    maybeToList,
    )
import JCSolver.Syncable
import JCSolver.Completable

type ForkFunction a = a -> [[a]]

type TransformFunction a = a -> Maybe a

slLoop :: Eq a => TransformFunction a -> TransformFunction a
slLoop f x = do
    x' <- f x
    if x == x' then return x else slLoop f x'

slSmartLoop :: (Completable a, Eq a) => TransformFunction a -> TransformFunction a
slSmartLoop f x
    | clIsCompleted x = return x
    | otherwise = do
        x' <- f x
        if x == x' then return x else slLoop f x'

slForkAndSyncAll :: (Syncable a) => ForkFunction a -> TransformFunction a -> TransformFunction a
slForkAndSyncAll f g x = do
    xs <- mapM (snAverageAll . mapMaybe g) $ f x
    snSyncAll (x : xs)

slForkAndSmartSync :: (Syncable a, Completable a, Eq a) => ForkFunction a -> TransformFunction a -> TransformFunction a
slForkAndSmartSync f g x = foldr h (return x) (f x) where
    h xs mx = do
        x' <- mx
        if clIsCompleted x' then mx else case mapMaybe (snSync x') xs of
            [] -> Nothing
            xs' -> case filter (/= x') xs' of
                [] -> return x'
                xs'' -> snAverageAll . mapMaybe g $ xs''

slAllSolutions :: (Completable a) => ForkFunction a -> TransformFunction a -> a -> [a]
slAllSolutions f g x = do
    x' <- maybeToList $ g x
    if clIsCompleted x' then return x' else case f x' of
        (xs : _) -> do
            x'' <- xs
            slAllSolutions f g x''
        [] -> []
