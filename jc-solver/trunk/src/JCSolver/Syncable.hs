module JCSolver.Syncable (
    Syncable (..),
    ) where

import JCSolver.Utils

class Syncable a where
    snSync :: a -> a -> Maybe a
    sn1 `snSync` sn2 = snSyncAll [sn1, sn2]
    snAverage :: a -> a -> Maybe a
    sn1 `snAverage` sn2 = snAverageAll [sn1, sn2]
    snSyncAll :: [a] -> Maybe a
    snSyncAll [] = Nothing
    snSyncAll sns = foldr1 (wrap snSync) (map return sns)
    snAverageAll :: [a] -> Maybe a
    snAverageAll [] = Nothing
    snAverageAll sns = foldr1 (wrap snAverage) (map return sns)
