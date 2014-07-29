-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

module Curve25519
    ( FieldP
    , FieldPSq
    , sqrt2
    , fromFieldP
    , Point (InfPt, Pt)
    , fromPointP
    , validPt
    , negPt
    , (.+)
    , (.-)
    , doublePt
    , sumPts
    , combinePts
    , (.*)
    , dhArith
    ) where

import Data.Ratio (numerator, denominator)
import Math.NumberTheory.Moduli (sqrtModP)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

inv :: Integer -> Integer -> Integer
inv = xEuclid 1 0 0 1 where
    xEuclid x0 y0 x1 y1 u v
        | v == 0 = x0
        | otherwise =
            let (q , r) = u `divMod` v
            in xEuclid x1 y1 (x0-q*x1) (y0-q*y1) v r

newtype FieldP = FieldP Integer
    deriving (Eq, Show)

p :: Integer
p = 57896044618658097711785492504343953926634992332820282019728792003956564819949

instance Num FieldP where
    (FieldP x) + (FieldP x') = FieldP ((x + x') `mod` p)
    (FieldP x) * (FieldP x') = FieldP ((x * x') `mod` p)
    negate (FieldP x) = FieldP ((negate x) `mod` p)
    fromInteger i = FieldP (i `mod` p)
    abs = id
    signum = const 1

instance Fractional FieldP where
    recip (FieldP x) = FieldP (inv x p)
    fromRational q
        = fromInteger (numerator q)
        / fromInteger (denominator q)

data FieldPSq = FieldPSq FieldP FieldP
    deriving (Eq, Show)

sqrt2 :: FieldPSq
sqrt2 = FieldPSq 0 1

fromFieldP :: FieldP -> FieldPSq
fromFieldP x = FieldPSq x 0

instance Num FieldPSq where
    (FieldPSq x y) + (FieldPSq x' y')
        = FieldPSq (x + x') (y + y')
    (FieldPSq x y) * (FieldPSq x' y')
        = FieldPSq (x*x' + 2*y*y') (x*y' + y*x')
    negate (FieldPSq x y)
        = FieldPSq (negate x) (negate y)
    fromInteger = fromFieldP . fromInteger
    abs = id
    signum = const 1

instance Fractional FieldPSq where
    recip (FieldPSq x y)
        = FieldPSq (x / (x^2 - 2*y^2)) ((-y) / (x^2 - 2*y^2))
    fromRational q
        = fromInteger (numerator q)
        / fromInteger (denominator q)

data Point k = InfPt | Pt k k
    deriving (Eq, Show)

fromPointP :: Point FieldP -> Point FieldPSq
fromPointP InfPt = InfPt
fromPointP (Pt x y) = Pt (fromFieldP x) (fromFieldP y)

a :: (Eq k, Fractional k) => k
a = 486662

validPt :: (Eq k, Fractional k) => Point k -> Bool
validPt InfPt = True
validPt (Pt x y) = y^2 == x^3 + a * x^2 + x

negPt :: (Eq k, Fractional k) => Point k -> Point k
negPt InfPt = InfPt
negPt (Pt x y) = Pt x (negate y)

(.+) :: (Eq k, Fractional k) => Point k -> Point k -> Point k
infixr 6 .+
InfPt .+ pt = pt
pt .+ InfPt = pt
pt@(Pt x y) .+ pt'@(Pt x' y')
    = if pt == negPt pt' then InfPt else
        let m = if pt == pt'
                then (3*x^2+2*a*x+1)/(2*y)
                else (y'-y)/(x'-x)
            x'' = m^2-a-x-x'
            y'' = m*(x-x'')-y
        in Pt x'' y''

(.-) :: (Eq k, Fractional k) => Point k -> Point k -> Point k
infixl 6 .-
pt .- pt' = pt .+ (negPt pt')

doublePt :: (Eq k, Fractional k) => Point k -> Point k
doublePt pt = pt .+ pt

sumPts :: (Eq k, Fractional k) => [Point k] -> Point k
sumPts = foldr (.+) InfPt

--Calculate a linear combination of points with integer coefficients
combinePts :: (Eq k, Fractional k) => [(Integer , Point k)] -> Point k
combinePts [] = InfPt
combinePts terms
    | all ((>= 0) . fst) terms
    = let terms' = [(n , pt) | (n , pt) <- terms, n /= 0, pt /= InfPt]
        in doublePt (combinePts [(n `div` 2 , pt) | (n , pt) <- terms'])
            .+ sumPts [pt | (n , pt) <- terms', odd n]
    | otherwise
    = combinePts [(abs n , if n < 0 then negPt pt else pt) | (n , pt) <- terms]

(.*) :: (Eq k, Fractional k) => Integer -> Point k -> Point k
infixr 7 .*
n .* pt = combinePts [(n , pt)]

basePt :: (Eq k, Fractional k) => Point k
basePt = Pt 9 14781619447589544791020593568409986887264606134616475288964881837755586237401

x0 :: Point FieldPSq -> FieldPSq
x0 InfPt = 0
x0 (Pt x _) = x

maybeY1 :: FieldP -> Maybe FieldPSq
maybeY1 x = fmap fromInteger (sqrtModP ySq p)
  where
    FieldP ySq = x^3 + a*x^2 + x

maybeY2 :: FieldP -> Maybe FieldPSq
maybeY2 x = fmap ((sqrt2 *) . fromInteger) (sqrtModP ySqHlf p)
  where
    FieldP ySqHlf = (x^3 + a*x^2 + x) / 2

maybeY :: FieldP -> Maybe FieldPSq
maybeY x = if x == 0 then Just 0 else (maybeY1 x <|> maybeY2 x)

unsafeY :: FieldP -> FieldPSq
unsafeY = fromJust . maybeY

castDown :: FieldPSq -> FieldP
castDown (FieldPSq x _) = x

dhArith :: Integer -> FieldP -> FieldP
dhArith sk pk = castDown . x0 $
    sk .* (Pt (fromFieldP pk) (unsafeY pk))
