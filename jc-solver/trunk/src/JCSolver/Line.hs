module JCSolver.Line (
    -- * 'Line' type
    Line (),
    lnCreate,
    -- * Getters
    lnMask,
    lnBlocks,
    -- * Modification
    lnSyncWithLineMask,
    -- * Solve
    lnSimpleTransform,
    lnTransformByExtremeOwners,
    lnForkByOwners,
    lnForkByCells,
    ) where

import Control.Monad (
    foldM,
    (>=>),
    )
import Data.List (
    findIndices,
    findIndex,
    )
import Data.Maybe (
    fromJust,
    maybeToList,
    )
import Data.Function (
    on,
    )
import JCSolver.BitMask
import JCSolver.LineMask
import JCSolver.Block
import JCSolver.Utils
import JCSolver.Solve

data Line = Line {
    lnMask :: LineMask,
    lnBlocks :: [Block]
    } deriving Eq

instance Show Line where
    show ln = unlines $ show (lnMask ln) : map show (lnBlocks ln)

instance Completable Line where
    clIsCompleted ln = all clIsCompleted (lnBlocks ln)

instance Syncable Line where
    ln1 `snSync` ln2 = do
        bls <- unsafeZipWithM snSync (lnBlocks ln1) (lnBlocks ln2)
        lm <- lnMask ln1 `snSync` lnMask ln2
        lnEnsureConsistency $ Line lm bls
    ln1 `snAverage` ln2 = do
        bls <- unsafeZipWithM snAverage (lnBlocks ln1) (lnBlocks ln2)
        lm <- lnMask ln1 `snAverage` lnMask ln2
        lnEnsureConsistency $ Line lm bls

lnUpdateBlocked :: [Block] -> TransformFunction LineMask
lnUpdateBlocked [] lm = lmBlock (bmNot $ lmBlockedMask lm) lm
lnUpdateBlocked bls lm = lmBlock (bmNot $ bmUnion $ map blScopeMask bls) lm

lnUpdateFilled :: [Block] -> TransformFunction LineMask
lnUpdateFilled [] = return
lnUpdateFilled bls = lmFill (bmUnion $ map blToFillMask bls)

lnEnsureConsistency :: TransformFunction Line
lnEnsureConsistency ln = do
    let bls = lnBlocks ln
    lm <- lnUpdateBlocked bls >=> lnUpdateFilled bls $ lnMask ln
    return $ ln { lnMask = lm }

lnCreate :: Int -> [Int] -> Maybe Line
lnCreate len nums = do
    bls <- mapM (blCreate len) nums
    lnEnsureConsistency $ Line (lmCreate len) bls

lnSyncWithLineMask :: LineMask -> TransformFunction Line
lnSyncWithLineMask lm ln = do
    lm' <- lm `snSync` lnMask ln
    return ln { lnMask = lm' }

lnRemoveBlocked :: LineMask -> TransformFunction [Block]
lnRemoveBlocked = mapM . blExclude . lmBlockedMask

lnRemoveFilled :: LineMask -> TransformFunction [Block]
lnRemoveFilled lm = mapM (\ bl -> foldM f bl $ bmSplit $ lmFilledMask lm) where
    f bl bm = if blCanContainMask bm bl then return bl else blExclude (bmExpand bm) bl

lnExcludeNeighbours :: TransformFunction [Block]
lnExcludeNeighbours bls = sequence $
    scanr1 (flip $ wrap $ blExclude . bmExpand . blMinimumRightMask) $
    scanl1 (wrap $ blExclude . bmExpand . blMinimumLeftMask) $
    map return bls

lnSimpleTransform :: TransformFunction Line
lnSimpleTransform ln = do
    let lm = lnMask ln
    bls <- lnRemoveBlocked lm >=> slLoop (lnRemoveFilled lm >=> lnExcludeNeighbours) $ lnBlocks ln
    lnEnsureConsistency ln { lnBlocks = bls }

lnExtremeOwners :: BitMask -> TransformFunction [Block]
lnExtremeOwners bm bls = do
    bls' <- fmap reverse $ maybe (return bls) (f bmLeftIncursion bls) (h bls)
    fmap reverse $ maybe (return bls') (f bmRightIncursion bls') (h bls')
    where
        f g = varyNth (\ bl -> blKeep (g (blNumber bl) bm) bl)
        h = findIndex (blCanContainMask bm)

lnTransformByExtremeOwners :: TransformFunction Line
lnTransformByExtremeOwners ln = do
    bls <- foldM (flip lnExtremeOwners) (lnBlocks ln) $ bmSplit $ lmFilledMask $ lnMask ln
    lnEnsureConsistency ln { lnBlocks = bls }

lnForkByOwners :: ForkFunction Line
lnForkByOwners ln = do
    let bls = lnBlocks ln
    bm <- bmSplit $ lmFilledMask $ lnMask ln
    case findIndices (blCanContainMask bm) bls of
        [_] -> []
        idxs -> return $ do
            idx <- idxs
            maybeToList $ do
                bls' <- varyNth (g bm) bls idx
                lnEnsureConsistency ln { lnBlocks = bls' }
    where g bm bl = blKeep ((bmAnd `on` ($ bm) . ($ blNumber bl)) bmLeftIncursion bmRightIncursion) bl

lnForkByCells :: ForkFunction Line
lnForkByCells ln = do
    let lm = lnMask ln
    bm <- bmByOne $ lmEmptyMask lm
    return $ do
        lm' <- [fromJust $ lmBlock bm lm, fromJust $ lmFill bm lm]
        maybeToList $ lnEnsureConsistency ln { lnMask = lm' }
