{-# LANGUAGE TupleSections #-}

module FilterComonad where

import Control.Comonad
import Data.Bifunctor (Bifunctor(second))
import Data.Complex (Complex(..))
import GHC.Float (int2Double)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

data TorusZipper a = TZ a [a] deriving Show

fromList2TZ xs = TZ (head xs) (tail xs)

next (TZ n ns) = TZ (head ns) $ tail ns ++ [n]

instance Functor TorusZipper where
    fmap f (TZ n ns) = TZ (f n) (fmap f ns)

instance Comonad TorusZipper where
    extract (TZ n _) = n
    duplicate tz@(TZ n ns) = TZ tz $ tail . take (1 + length ns) . iterate next $ tz

neighborMean :: Fractional a => TorusZipper a -> a
neighborMean (TZ n (x:xs)) = (n + x) / 2
neighborMean (TZ n []) = n

applyFilter :: Comonad w => (w a -> b) -> w a -> w b
applyFilter = extend

newtype IndexedList a = IL [(Int, a)] deriving Show

instance Functor IndexedList where
    fmap f (IL ixs) = IL $ map (second f) ixs

rotate :: Int -> [b] -> [b]
rotate n xs = drop n xs ++ take n xs

instance Comonad IndexedList where
    extract (IL ixs) = snd . head $ ixs
    duplicate il@(IL ixs) = IL $ map (\idx -> (idx, IL $ rotate idx ixs)) [0..length ixs - 1]

dft :: IndexedList (Complex Double) -> Complex Double
dft (IL ixs) = let cidx = int2Double $ fst $ head ixs
                   l = int2Double $ length ixs
                   phase = 2 * pi * cidx / l
               in sum $ map (\(idx, x) -> x * exp (0 :+ phase * int2Double idx)) ixs

bitRev :: Int -> IndexedList a -> a
bitRev s (IL ixs) = let (hidx, hx) = head ixs
                        bin = reverse $ map (read . pure :: Char -> Int) $ showIntAtBase 2 intToDigit hidx ""
                        lg = round $ logBase 2 (int2Double s) - 1
                        revIdx = sum $ zipWith (*) bin [2 ^ i | i <- reverse [0..lg]]
                    in snd (ixs !! mod (revIdx - hidx) s)

butterFlyOpt :: Int -> IndexedList (Complex Double) -> Complex Double
butterFlyOpt m (IL ixs) = let (hidx, hx) = head ixs
                              omega = exp (0 :+ (- pi / int2Double m * int2Double (mod hidx m)))
                              l = length ixs
                          in if even $ div hidx m
                            then hx + omega * snd (ixs !! m)
                            else snd (ixs !! (l - m)) - omega * hx

fft :: IndexedList (Complex Double) -> Complex Double
fft il@(IL ixs) = foldr (\idx cum -> cum =>= butterFlyOpt idx) (bitRev l) idxs il
    where
        l = length ixs
        lg = round $ logBase 2 (int2Double l) - 1
        idxs = [2 ^ i | i <- [0..lg]]