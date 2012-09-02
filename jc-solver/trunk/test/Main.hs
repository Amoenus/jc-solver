module Main (
    main,
    ) where

import Test.Framework (
    Test,
    defaultMain,
    testGroup,
    )
import Test.Framework.Providers.QuickCheck2 (
    testProperty,
    )
import Test.QuickCheck
import Data.Function (
    on,
    )
import JCSolver.BitMask

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    bitMaskTests
    ]

bitMaskTests :: Test
bitMaskTests = testGroup "BitMask" [
    testProperty "bmNot" prop_bmNot,
    testProperty "bmLength" prop_bmLength,
    testProperty "bmAnd size" prop_bmAnd_size,
    testGroup "bmSplit" [
        testProperty "length" $ prop_bmGenericSplit_length bmSplit,
        testProperty "size" $ prop_bmGenericSplit_size bmSplit,
        testProperty "union" $ prop_bmGenericSplit_union bmSplit,
        testProperty "intersection" $ prop_bmGenericSplit_intersection bmSplit,
        testProperty "idempotent" $ prop_bmGenericSplit_idempotent bmSplit
        ],
    testGroup "bmByOne" [
        testProperty "length" $ prop_bmGenericSplit_length bmByOne,
        testProperty "size" $ prop_bmGenericSplit_size bmByOne,
        testProperty "union" $ prop_bmGenericSplit_union bmByOne,
        testProperty "intersection" $ prop_bmGenericSplit_intersection bmByOne,
        testProperty "idempotent" $ prop_bmGenericSplit_idempotent bmByOne,
        testProperty "definite size" prop_bmByOne_definite_size,
        testProperty "count" prop_bmByOne_count
        ],
    testGroup "bmFrame" [
        testProperty "or" prop_bmFrame_or,
        testProperty "and" prop_bmFrame_and
        ]
    ]


prop_bmNot :: BitMask -> Bool
prop_bmNot bm = (bmNot . bmNot) bm == bm

prop_bmLength :: BitMask -> Bool
prop_bmLength bm = bmLength bm == bmSize bm + bmSize (bmNot bm)

prop_bmAnd_size :: BitMask -> BitMask -> Property
prop_bmAnd_size bm1 bm2 = ((==) `on` bmLength) bm1 bm2 ==> size <= bmSize bm1 && size <= bmSize bm2
    where size = bmSize (bm1 `bmAnd` bm2)


-- Каждая маска в разбиении имеет ту же длину, что и исходная
prop_bmGenericSplit_length :: (BitMask -> [BitMask]) -> BitMask -> Bool
prop_bmGenericSplit_length f bm = all ((== len) . bmLength) (f bm)
    where len = bmLength bm

-- Сумма размеров масок разбиения равна размеру исходной маски
prop_bmGenericSplit_size :: (BitMask -> [BitMask]) -> BitMask -> Bool
prop_bmGenericSplit_size f bm = sum (map bmSize $ f bm) == bmSize bm

-- Объединение масок разбиения образует исходную маску
prop_bmGenericSplit_union :: (BitMask -> [BitMask]) -> BitMask -> Property
prop_bmGenericSplit_union f bm = bmSize bm > 0 ==> bmUnion (f bm) == bm

-- Пересечение любой пары масок разбиения образует пустую маску
prop_bmGenericSplit_intersection :: (BitMask -> [BitMask]) -> BitMask -> Bool
prop_bmGenericSplit_intersection f bm = all (bmIsEmpty . uncurry bmAnd) [(bm1, bm2) | bm1 <- splitted, bm2 <- splitted, bm1 /= bm2]
    where splitted = f bm

-- Ни одна из масок разбиения не может быть разбита сама
prop_bmGenericSplit_idempotent :: (BitMask -> [BitMask]) -> BitMask -> Bool
prop_bmGenericSplit_idempotent f bm = concatMap f splitted == splitted
    where splitted = f bm


-- Размер каждой маски разбиения по одному равен 1
prop_bmByOne_definite_size :: BitMask -> Bool
prop_bmByOne_definite_size bm = all ((== 1) . bmSize) (bmByOne bm)

-- Количество масок в разбиении по одному равно размеру исходной маски
prop_bmByOne_count :: BitMask -> Bool
prop_bmByOne_count bm = length (bmByOne bm) == bmSize bm


-- Объединение рамки маски с самой маской равно расширенной маске
prop_bmFrame_or :: BitMask -> Bool
prop_bmFrame_or bm = bmFrame bm `bmOr` bm == bmExpand bm

-- Пересечение маски со своей рамкой пусто
prop_bmFrame_and :: BitMask -> Bool
prop_bmFrame_and bm = bmIsEmpty $ bmFrame bm `bmAnd` bm
