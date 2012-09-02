module JCSolver.BitMask (
    -- * 'BitMask' type
    BitMask (),
    bmCreate,
    -- * Getters
    bmBools,
    bmLength,
    bmSize,
    bmIsEmpty,
    -- * Modification
    -- ** Bitwise
    bmNot,
    bmAnd,
    bmOr,
    bmIntersection,
    bmUnion,
    -- ** Section
    bmSplit,
    bmByOne,
    bmExpand,
    bmFrame,
    bmFillGaps,
    -- ** Incursion
    bmLeftIncursion,
    bmRightIncursion,
    -- ** Other
    bmTranspose,
    ) where

import Test.QuickCheck (
    Arbitrary (..)
    )
import Data.List (
    mapAccumL,
    mapAccumR,
    )
import JCSolver.Utils

newtype BitMask = BitMask {
    bmBools :: [Bool]
    } deriving Eq

instance Show BitMask where
    show = map (\ b -> if b then '+' else '-') . bmBools

instance Arbitrary BitMask where
    arbitrary = do
        bs <- arbitrary
        return $ BitMask bs

bmCreate :: Int -> BitMask
bmCreate len = BitMask $ replicate len False

bmLength :: BitMask -> Int
bmLength = length . bmBools

bmSize :: BitMask -> Int
bmSize = length . filter id . bmBools

bmIsEmpty :: BitMask -> Bool
bmIsEmpty = not . or . bmBools

bmNot :: BitMask -> BitMask
bmNot = BitMask . map not . bmBools

bmAnd :: BitMask -> BitMask -> BitMask
bmAnd bm1 bm2 = BitMask $ unsafeZipWith (&&) (bmBools bm1) (bmBools bm2)

bmOr :: BitMask -> BitMask -> BitMask
bmOr bm1 bm2 = BitMask $ unsafeZipWith (||) (bmBools bm1) (bmBools bm2)

bmIntersection :: [BitMask] -> BitMask
bmIntersection = foldr1 bmAnd

bmUnion :: [BitMask] -> BitMask
bmUnion = foldr1 bmOr

bmSplit :: BitMask -> [BitMask]
bmSplit = map BitMask . fst3 . foldr f ([], [], False) . bmBools where
    f True (bss, bs, True) = ((True : head bss) : map (False :) (tail bss), False : bs, True)
    f True (bss, bs, False) = ((True : bs) : map (False :) bss, False : bs, True)
    f False (bss, bs, _) = (map (False :) bss, False : bs, False)
    fst3 (x, _, _) = x

bmByOne :: BitMask -> [BitMask]
bmByOne = map BitMask . fst . foldr f ([], []) . bmBools where
    f True (bss, bs) = ((True : bs) : map (False :) bss, False : bs)
    f False (bss, bs) = (map (False :) bss, False : bs)

bmExpand :: BitMask -> BitMask
bmExpand = BitMask . f . bmBools where
    f (False : bs @ (True : _)) = True : f bs
    f (True : False : bs) = True : True : f bs
    f (b : bs) = b : f bs
    f [] = []

bmFrame :: BitMask -> BitMask
bmFrame = BitMask . f . bmBools where
    f (False : bs @ (True : _)) = True : f bs
    f (True : False : bs) = False : True : f bs
    f (_ : bs) = False : f bs
    f [] = []

bmFillGaps :: BitMask -> BitMask
bmFillGaps = BitMask . f . bmBools where
    f (False : bs) = False : f bs
    f (True : bs) = True : g bs
    f [] = []
    g [b] = [b]
    g (b : bs) = let bs' @ (b' : _) = g bs in (b || b') : bs'
    g [] = []

type MapAccumFunc acc x y = (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])

bmIncursion :: MapAccumFunc (Maybe Int) Bool Bool -> Int -> BitMask -> BitMask
bmIncursion mapAccumFunc num = BitMask . snd . mapAccumFunc f Nothing . bmBools where
    f Nothing False = (Nothing, True) 
    f Nothing True = f (Just num) True
    f (Just num') _ = (Just (num' - 1), num' > 0)

bmLeftIncursion :: Int -> BitMask -> BitMask
bmLeftIncursion = bmIncursion mapAccumL

bmRightIncursion :: Int -> BitMask -> BitMask
bmRightIncursion = bmIncursion mapAccumR

bmTranspose :: [BitMask] -> [BitMask]
bmTranspose bms = map BitMask $ unsafeTranspose $ map bmBools bms
