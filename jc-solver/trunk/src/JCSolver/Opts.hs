module JCSolver.Opts (
    -- * 'Opts' type
    Opts (),
    Mode (..),
    -- * Getters
    optMode,
    optInput,
    optOutput,
    optHelp,
    -- * Other
    optParse,
    optUsage,
    ) where

import Data.Maybe (
    mapMaybe,
    listToMaybe,
    )
import System.Console.GetOpt

data Opts = Opts {
    optMode :: Mode,
    optInput :: Maybe String,
    optOutput :: Maybe String,
    optHelp :: Bool
    }

data Opt = OptHelp | OptInput String | OptOutput String | OptMode String deriving Eq

data Mode = ModeSolve | ModePbm2Cond

optDescrs :: [OptDescr Opt]
optDescrs = [
    Option "m" ["mode"] (ReqArg OptMode "MODE") $ unlines [
        "where MODE is one of:",
        "solve (default) - consume puzzle conditions and produce all solutions as pbm",
        "pbm2cond - consume pbm (first image in file is taken) and produce puzzle conditions"
        ],
    Option "i" ["input"] (ReqArg OptInput "FILE") "input from FILE (stdin by default)",
    Option "o" ["output"] (ReqArg OptOutput "FILE") "output to FILE (stdout by default)",
    Option "h?" ["help"] (NoArg OptHelp) "print this message to stderr and exit"
    ]

optParse :: [String] -> Maybe Opts
optParse args = do
    opts <- case getOpt Permute optDescrs args of
        (opts, [], []) -> return opts
        _ -> Nothing
    mode <- case listToMaybe $ mapMaybe (\ opt -> case opt of { OptMode s -> Just s; _ -> Nothing }) opts of
        Nothing -> Just ModeSolve
        Just "solve" -> Just ModeSolve
        Just "pbm2cond" -> Just ModePbm2Cond
        _ -> Nothing
    return $ Opts
        mode
        (listToMaybe $ mapMaybe (\ opt -> case opt of { OptInput s -> Just s; _ -> Nothing }) opts)
        (listToMaybe $ mapMaybe (\ opt -> case opt of { OptOutput s -> Just s; _ -> Nothing }) opts)
        (OptHelp `elem` opts)
    
optUsage :: String
optUsage = usageInfo "Usage: jc-solver [OPTION...] (for duplicate options first occurrence only matters)" optDescrs
