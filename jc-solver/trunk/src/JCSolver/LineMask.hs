module JCSolver.LineMask (
    -- * 'LineMask' type
    LineMask (),
    lmCreate,
    -- * Getters
    lmFilledMask,
    lmBlockedMask,
    lmEmptyMask,
    lmCells,
    -- * Modification
    lmFill,
    lmBlock,
    lmTranspose,
    ) where

import Control.Monad (
    guard,
    )
import JCSolver.BitMask
import JCSolver.Solve
import JCSolver.Cell

data LineMask = LineMask {
    lmFilledMask :: BitMask,
    lmBlockedMask :: BitMask
    } deriving Eq

instance Show LineMask where
    show = concatMap show . lmCells

instance Completable LineMask where
    clIsCompleted = bmIsEmpty . lmEmptyMask

instance Syncable LineMask where
    lm1 `snSync` lm2 = lmEnsureConsistency $ LineMask
        (lmFilledMask lm1 `bmOr` lmFilledMask lm2)
        (lmBlockedMask lm1 `bmOr` lmBlockedMask lm2)
    lm1 `snAverage` lm2 = lmEnsureConsistency $ LineMask
        (lmFilledMask lm1 `bmAnd` lmFilledMask lm2)
        (lmBlockedMask lm1 `bmAnd` lmBlockedMask lm2)

lmEnsureConsistency :: TransformFunction LineMask
lmEnsureConsistency lm = do
    guard $ bmIsEmpty $ lmFilledMask lm `bmAnd` lmBlockedMask lm
    return lm

lmCreate :: Int -> LineMask
lmCreate len = let bm = bmCreate len in LineMask bm bm

lmEmptyMask :: LineMask -> BitMask
lmEmptyMask lm = bmNot $ lmFilledMask lm `bmOr` lmBlockedMask lm

lmCells :: LineMask -> [Cell]
lmCells lm = zipWith f (bmBools $ lmFilledMask lm) (bmBools $ lmBlockedMask lm) where
    f True _ = CellFilled
    f _ True = CellBlocked
    f _ _ = CellEmpty

lmFill :: BitMask -> TransformFunction LineMask
lmFill bm lm = lmEnsureConsistency lm { lmFilledMask = lmFilledMask lm `bmOr` bm }

lmBlock :: BitMask -> TransformFunction LineMask
lmBlock bm lm = lmEnsureConsistency lm { lmBlockedMask = lmBlockedMask lm `bmOr` bm }

lmTranspose :: [LineMask] -> [LineMask]
lmTranspose lms = zipWith LineMask
    (bmTranspose $ map lmFilledMask lms)
    (bmTranspose $ map lmBlockedMask lms)
