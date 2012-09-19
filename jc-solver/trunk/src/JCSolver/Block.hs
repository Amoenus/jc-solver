module JCSolver.Block(
    -- * 'Block' type
    Block (),
    blCreate,
    -- * Getters
    blScopeMask,
    blNumber,
    blMinimumLeftMask,
    blMinimumRightMask,
    blToFillMask,
    -- * Modification
    blExclude,
    blKeep,
    -- * Other
    blCanContainMask,
    ) where

import Control.Monad (
    guard,
    )
import JCSolver.BitMask
import JCSolver.Solve

data Block = Block {
    blScopeMask :: BitMask,
    blNumber :: Int
    } deriving Eq

instance Show Block where
    show bl = show (blScopeMask bl) ++ ' ' : show (blNumber bl)

instance Completable Block where
    clIsCompleted bl = bmSize (blScopeMask bl) == blNumber bl

instance Syncable Block where
    bl1 `snSync` bl2
        | blNumber bl1 /= blNumber bl2 = undefined
        | otherwise = blEnsureConsistency bl1 { blScopeMask = blScopeMask bl1 `bmAnd` blScopeMask bl2 }
    bl1 `snAverage` bl2
        | blNumber bl1 /= blNumber bl2 = undefined
        | otherwise = blEnsureConsistency bl1 { blScopeMask = blScopeMask bl1 `bmOr` blScopeMask bl2 }

blEnsureConsistency :: TransformFunction Block
blEnsureConsistency bl = do
    let bms = filter ((blNumber bl <=) . bmSize) $ bmSplit $ blScopeMask bl
    guard $ not $ null bms
    return bl { blScopeMask = bmUnion bms }

blCreate :: Int -> Int -> Maybe Block
blCreate len num = do
    guard $ num > 0 && num <= len
    return $ Block (bmNot (bmCreate len)) num

blMinimumLeftMask :: Block -> BitMask
blMinimumLeftMask bl = bmLeftIncursion (blNumber bl) (blScopeMask bl)

blMinimumRightMask :: Block -> BitMask
blMinimumRightMask bl = bmRightIncursion (blNumber bl) (blScopeMask bl)

blToFillMask :: Block -> BitMask
blToFillMask bl = blMinimumLeftMask bl `bmAnd` blMinimumRightMask bl

blExclude :: BitMask -> TransformFunction Block
blExclude bm bl = blEnsureConsistency $ bl { blScopeMask = blScopeMask bl `bmAnd` bmNot bm }

blKeep :: BitMask -> TransformFunction Block
blKeep bm bl = blEnsureConsistency $ bl { blScopeMask = blScopeMask bl `bmAnd` bm }

blCanContainMask :: BitMask -> Block -> Bool
blCanContainMask bm bl =
    let bm' = bmFillGaps bm
    in bmSize bm' <= blNumber bl && bmIsEmpty (bm' `bmAnd` bmNot (blScopeMask bl))
