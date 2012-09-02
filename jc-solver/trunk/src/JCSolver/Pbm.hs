module JCSolver.Pbm (
    -- * 'Pbm' type
    Pbm (),
    pbmCreate,
    -- * Getters
    pbmWidth,
    pbmHeight,
    pbmRaster,
    -- * Other
    pbmImport,
    pbmExport,
    ) where

import Control.Monad (
    void,
    liftM,
    )
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Data.Bits (
    testBit,
    bit,
    (.|.),
    )
import Data.List (
    intercalate,
    )
import Data.Char (
    chr,
    )

data Pbm = Pbm {
    pbmWidth :: Int,
    pbmHeight :: Int,
    pbmRaster :: [[Bool]]
    }

pbmCreate :: Int -> Int -> [[Bool]] -> Pbm
pbmCreate width height raster = Pbm width height $
    map (take width . (++ repeat False)) $
    take height $ raster ++ repeat []

pbmImport :: B.ByteString -> [Pbm]
pbmImport byteString = case parse (many imageParser) "" byteString of
    Left _ -> []
    Right pbms -> pbms

pbmExport :: Pbm -> B.ByteString
pbmExport pbm = B.pack $ intercalate [charLF] [stringP4, size, raster] where
    size = concat [show (pbmWidth pbm), [charSpace], show (pbmHeight pbm)]
    raster = concatMap boolsToChars (pbmRaster pbm)

imageParser :: Parser Pbm
imageParser = do
    pbmMagickParser
    _ <- spaceParser
    width <- sizeParser
    _ <- spaceParser
    height <- sizeParser
    _ <- many commentParser
    spaceCharParser
    content <- count height $ count (let (d, m) = width `divMod` 8 in d + signum m) anyChar
    return $ pbmCreate width height $ map charsToBools content

charTAB :: Char
charTAB = '\x9'
charLF :: Char
charLF = '\xA'
charVT :: Char
charVT = '\xB'
charFF :: Char
charFF = '\xC'
charCR :: Char
charCR = '\xD'
charSpace :: Char
charSpace = '\x20'
charSharp :: Char
charSharp = '\x23'
stringP4 :: String
stringP4 = "\x50\x34"

spaceCharParser :: Parser ()
spaceCharParser = void $ oneOf [charTAB, charLF, charVT, charFF, charCR, charSpace]

commentParser :: Parser String
commentParser = char charSharp >> manyTill anyChar (oneOf [charLF, charCR])

spaceParser :: Parser [String]
spaceParser = (spaceCharParser >> spaceParser)
    <|> do { c <- commentParser; cs <- spaceParser; return (c : cs) }
    <|> return []

pbmMagickParser :: Parser ()
pbmMagickParser = void $ string stringP4

sizeParser :: Parser Int
sizeParser = liftM read (many1 digit)

charsToBools :: String -> [Bool]
charsToBools = concatMap (\ c -> map (testBit $ fromEnum c) [7, 6 .. 0])

boolsToChars :: [Bool] -> String
boolsToChars [] = []
boolsToChars bs =
    let (bs1, bs2) = splitAt 8 bs
    in chr (foldr1 (.|.) $ zipWith (\ b n -> bit $ if b then n else -1) bs1 [7, 6 .. 0])
        : boolsToChars bs2
