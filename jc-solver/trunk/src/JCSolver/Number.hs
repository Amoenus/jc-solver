module JCSolver.Number (
    -- * 'Number' type
    Number (),
    nmCreate,
    -- * Getters
    nmScopeMask,
    nmNumber,
    nmMinimumLeftMask,
    nmMinimumRightMask,
    nmToFillMask,
    -- * Modification
    nmExclude,
    nmKeep,
    -- * Other
    nmCanContainMask,
    ) where

import Control.Monad (
    guard,
    )
import JCSolver.BitMask
import JCSolver.Solve

data Number = Number {
    nmScopeMask :: BitMask,
    nmNumber :: Int
    } deriving Eq

instance Show Number where
    show nm = show (nmScopeMask nm) ++ ' ' : show (nmNumber nm)

instance Completable Number where
    clIsCompleted nm = bmSize (nmScopeMask nm) == nmNumber nm

instance Syncable Number where
    nm1 `snSync` nm2
        | nmNumber nm1 /= nmNumber nm2 = undefined
        | otherwise = nmEnsureConsistency nm1 { nmScopeMask = nmScopeMask nm1 `bmAnd` nmScopeMask nm2 }
    nm1 `snAverage` nm2
        | nmNumber nm1 /= nmNumber nm2 = undefined
        | otherwise = nmEnsureConsistency nm1 { nmScopeMask = nmScopeMask nm1 `bmOr` nmScopeMask nm2 }

nmEnsureConsistency :: TransformFunction Number
nmEnsureConsistency nm = do
    let bms = filter ((nmNumber nm <=) . bmSize) $ bmSplit $ nmScopeMask nm
    guard $ not $ null bms
    return nm { nmScopeMask = bmUnion bms }

nmCreate :: Int -> Int -> Maybe Number
nmCreate len num = do
    guard $ num > 0 && num <= len
    return $ Number (bmNot (bmCreate len)) num

nmMinimumLeftMask :: Number -> BitMask
nmMinimumLeftMask nm = bmLeftIncursion (nmNumber nm) (nmScopeMask nm)

nmMinimumRightMask :: Number -> BitMask
nmMinimumRightMask nm = bmRightIncursion (nmNumber nm) (nmScopeMask nm)

nmToFillMask :: Number -> BitMask
nmToFillMask nm = nmMinimumLeftMask nm `bmAnd` nmMinimumRightMask nm

nmExclude :: BitMask -> TransformFunction Number
nmExclude bm nm = nmEnsureConsistency $ nm { nmScopeMask = nmScopeMask nm `bmAnd` bmNot bm }

nmKeep :: BitMask -> TransformFunction Number
nmKeep bm nm = nmEnsureConsistency $ nm { nmScopeMask = nmScopeMask nm `bmAnd` bm }

nmCanContainMask :: BitMask -> Number -> Bool
nmCanContainMask bm nm =
    let bm' = bmFillGaps bm
    in bmSize bm' <= nmNumber nm && bmIsEmpty (bm' `bmAnd` bmNot (nmScopeMask nm))
