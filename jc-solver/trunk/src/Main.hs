module Main (
    main,
    ) where

import Control.Monad (
    (>=>),
    when,
    )
import Control.Exception (
    bracket,
    )
import Data.Maybe (
    maybeToList,
    )
import Data.List (
    transpose,
    group,
    )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.Exit
import System.IO
import JCSolver.Field
import JCSolver.Solve
import JCSolver.BitMask
import JCSolver.LineMask
import JCSolver.Line
import JCSolver.Pbm
import JCSolver.Opts

main :: IO ()
main = do
    args <- getArgs
    options <- case optParse args of
        Just opts -> return opts
        _ -> hPutStr stderr optUsage >> exitFailure
    when (optHelp options) (hPutStr stderr optUsage >> exitSuccess)
    bracket
        (maybe (return stdin) (`openFile` ReadMode) (optInput options))
        hClose
        $ \ inFile -> bracket
            (maybe (return stdout) (`openFile` WriteMode) (optOutput options))
            hClose
            $ \ outFile -> case optMode options of
                ModeSolve -> solveMode inFile outFile
                ModePbm2Cond -> pbm2CondMode inFile outFile

type ModeHandler = Handle -> Handle -> IO ()

solveMode :: ModeHandler
solveMode inFile outFile = do
    hSetBinaryMode inFile False
    hSetBinaryMode outFile True
    content <- hGetContents inFile
    let (hns, vns) = read content
    B.hPutStrLn outFile $ B.concat $ map (pbmExport . flToPbm)
        $ maybeToList (flCreate hns vns) >>= slAllSolutions flForkByCells fieldTransform''
    where
        lineTransform = slSmartLoop $ lnSimpleTransform >=> lnTransformByExtremeOwners
        lineTransform' = slForkAndSyncAll lnForkByOwners lineTransform
        fieldTransform = slSmartLoop $ slSmartLoop (flTransformByLines lineTransform) >=> flTransformByLines lineTransform'
        fieldTransform' = slForkAndSmartSync flForkByCells fieldTransform
        fieldTransform'' = slSmartLoop $ fieldTransform >=> fieldTransform'

pbm2CondMode :: ModeHandler
pbm2CondMode inFile outFile = do
    hSetBinaryMode inFile True
    hSetBinaryMode outFile False
    content <- B.hGetContents inFile
    let raster = pbmRaster$ head $ pbmImport content
    hPrint outFile (f raster, f (transpose raster))
    where f = map (map length . filter and . group)

flToPbm :: Field -> Pbm
flToPbm fl = 
    let
        lnsHor = flHorLines fl
        lnsVer = flVerLines fl
    in pbmCreate (length lnsVer) (length lnsHor) (map (bmBools . lmFilledMask . lnMask) lnsHor)
