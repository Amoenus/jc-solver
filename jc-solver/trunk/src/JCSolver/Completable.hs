module JCSolver.Completable (
    Completable (..),
    ) where

class Completable a where
    clIsCompleted :: a -> Bool
