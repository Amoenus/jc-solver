module JCSolver.Line (
    -- * 'Line' type
    Line (),
    lnCreate,
    -- * Getters
    lnMask,
    lnNumbers,
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
import JCSolver.Number
import JCSolver.Utils
import JCSolver.Solve

data Line = Line {
    lnMask :: LineMask,
    lnNumbers :: [Number]
    } deriving Eq

instance Show Line where
    show ln = unlines $ show (lnMask ln) : map show (lnNumbers ln)

instance Completable Line where
    clIsCompleted ln = all clIsCompleted (lnNumbers ln)

instance Syncable Line where
    ln1 `snSync` ln2 = do
        nms <- unsafeZipWithM snSync (lnNumbers ln1) (lnNumbers ln2)
        lm <- lnMask ln1 `snSync` lnMask ln2
        lnEnsureConsistency $ Line lm nms
    ln1 `snAverage` ln2 = do
        nms <- unsafeZipWithM snAverage (lnNumbers ln1) (lnNumbers ln2)
        lm <- lnMask ln1 `snAverage` lnMask ln2
        lnEnsureConsistency $ Line lm nms

lnUpdateBlocked :: [Number] -> TransformFunction LineMask
lnUpdateBlocked [] lm = lmBlock (bmNot $ lmBlockedMask lm) lm
lnUpdateBlocked nms lm = lmBlock (bmNot $ bmUnion $ map nmScopeMask nms) lm

lnUpdateFilled :: [Number] -> TransformFunction LineMask
lnUpdateFilled [] = return
lnUpdateFilled nms = lmFill (bmUnion $ map nmToFillMask nms)

lnEnsureConsistency :: TransformFunction Line
lnEnsureConsistency ln = do
    let nms = lnNumbers ln
    lm <- lnUpdateBlocked nms >=> lnUpdateFilled nms $ lnMask ln
    return $ ln { lnMask = lm }

lnCreate :: Int -> [Int] -> Maybe Line
lnCreate len nums = do
    nms <- mapM (nmCreate len) nums
    lnEnsureConsistency $ Line (lmCreate len) nms

lnSyncWithLineMask :: LineMask -> TransformFunction Line
lnSyncWithLineMask lm ln = do
    lm' <- lm `snSync` lnMask ln
    return ln { lnMask = lm' }

lnRemoveBlocked :: LineMask -> TransformFunction [Number]
lnRemoveBlocked = mapM . nmExclude . lmBlockedMask

lnRemoveFilled :: LineMask -> TransformFunction [Number]
lnRemoveFilled lm = mapM (\ nm -> foldM f nm $ bmSplit $ lmFilledMask lm) where
    f nm bm = if nmCanContainMask bm nm then return nm else nmExclude (bmExpand bm) nm

lnExcludeNeighbours :: TransformFunction [Number]
lnExcludeNeighbours nms = sequence $
    scanr1 (flip $ wrap $ nmExclude . bmExpand . nmMinimumRightMask) $
    scanl1 (wrap $ nmExclude . bmExpand . nmMinimumLeftMask) $
    map return nms

lnSimpleTransform :: TransformFunction Line
lnSimpleTransform ln = do
    let lm = lnMask ln
    nms <- lnRemoveBlocked lm >=> slLoop (lnRemoveFilled lm >=> lnExcludeNeighbours) $ lnNumbers ln
    lnEnsureConsistency ln { lnNumbers = nms }

lnExtremeOwners :: BitMask -> TransformFunction [Number]
lnExtremeOwners bm nms = do
    nms' <- fmap reverse $ maybe (return nms) (f bmLeftIncursion nms) (h nms)
    fmap reverse $ maybe (return nms') (f bmRightIncursion nms') (h nms')
    where
        f g = varyNth (\ nm -> nmKeep (g (nmNumber nm) bm) nm)
        h = findIndex (nmCanContainMask bm)

lnTransformByExtremeOwners :: TransformFunction Line
lnTransformByExtremeOwners = slLoop $ \ ln -> do
    nms <- foldM (flip lnExtremeOwners) (lnNumbers ln) $ bmSplit $ lmFilledMask $ lnMask ln
    lnEnsureConsistency ln { lnNumbers = nms }

lnForkByOwners :: ForkFunction Line
lnForkByOwners ln = do
    let nms = lnNumbers ln
    bm <- bmSplit $ lmFilledMask $ lnMask ln
    case findIndices (nmCanContainMask bm) nms of
        [_] -> []
        idxs -> return $ do
            idx <- idxs
            maybeToList $ do
                nms' <- varyNth (g bm) nms idx
                lnEnsureConsistency ln { lnNumbers = nms' }
    where g bm nm = nmKeep ((bmAnd `on` ($ bm) . ($ nmNumber nm)) bmLeftIncursion bmRightIncursion) nm

lnForkByCells :: ForkFunction Line
lnForkByCells ln = do
    let lm = lnMask ln
    bm <- bmByOne $ lmEmptyMask lm
    return $ do
        lm' <- [fromJust $ lmBlock bm lm, fromJust $ lmFill bm lm]
        maybeToList $ lnEnsureConsistency ln { lnMask = lm' }
