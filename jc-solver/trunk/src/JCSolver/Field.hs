module JCSolver.Field (
    -- * 'Field' type
    Field (),
    flCreate,
    -- * Getters
    flHorLines,
    flVerLines,
    -- * Solve
    flTransformByLines,
    flForkByCells,
    ) where

import Data.Maybe (
    maybeToList,
    )
import Data.List (
    findIndices,
    )
import Control.Monad (
    zipWithM,
    guard,
    )
import JCSolver.Line
import JCSolver.LineMask
import JCSolver.Solve
import JCSolver.Utils

data Field = Field {
    flHorLines :: [Line],
    flVerLines :: [Line]
    } deriving Eq

instance Show Field where
    show fl = unlines $ map show $ flHorLines fl ++ flVerLines fl

instance Completable Field where
    clIsCompleted fl = all clIsCompleted (flHorLines fl) && all clIsCompleted (flVerLines fl)

instance Syncable Field where
    fl1 `snSync` fl2 = do
        lnsHor <- unsafeZipWithM snSync (flHorLines fl1) (flHorLines fl2)
        lnsVer <- unsafeZipWithM snSync (flVerLines fl1) (flVerLines fl2)
        flEnsureConsistency $ Field lnsHor lnsVer
    fl1 `snAverage` fl2 = do
        lnsHor <- unsafeZipWithM snAverage (flHorLines fl1) (flHorLines fl2)
        lnsVer <- unsafeZipWithM snAverage (flVerLines fl1) (flVerLines fl2)
        flEnsureConsistency $ Field lnsHor lnsVer

flEnsureConsistency :: TransformFunction Field
flEnsureConsistency fl = do
    let lnsHor = flHorLines fl
    let lnsVer = flVerLines fl
    lnsHor' <- zipWithM lnSyncWithLineMask (lmTranspose $ map lnMask lnsVer) lnsHor
    lnsVer' <- zipWithM lnSyncWithLineMask (lmTranspose $ map lnMask lnsHor) lnsVer
    return $ Field lnsHor' lnsVer'

flCreate :: [[Int]] -> [[Int]] -> Maybe Field
flCreate numsHor numsVer = do
    guard $ sum (concat numsHor) == sum (concat numsVer)
    lnsHor <- mapM (lnCreate (length numsVer)) numsHor
    lnsVer <- mapM (lnCreate (length numsHor)) numsVer
    flEnsureConsistency $ Field lnsHor lnsVer

flTransformByLines :: TransformFunction Line -> TransformFunction Field 
flTransformByLines f fl = do
    lnsHor <- mapM f (flHorLines fl)
    fl' <- flEnsureConsistency fl { flHorLines = lnsHor }
    lnsVer <- mapM f (flVerLines fl')
    flEnsureConsistency fl' { flVerLines = lnsVer }

flForkByCells :: ForkFunction Field
flForkByCells fl = do
    let lnsHor = flHorLines fl
    let lnsVer = flVerLines fl
    idx <- findIndices (not . clIsCompleted) lnsHor
    let (lns1, ln : lns2) = splitAt idx lnsHor
    lns <- lnForkByCells ln
    return $ do
        ln' <- lns
        maybeToList $ flEnsureConsistency $ Field (lns1 ++ ln' : lns2) lnsVer
