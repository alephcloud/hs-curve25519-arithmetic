import Data.Ratio (numerator, denominator)
import qualified Data.ByteString as B
import Data.Bits

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

a :: FieldP
a = 486662

add :: (FieldP,FieldP) -> (FieldP,FieldP) -> (FieldP,FieldP) -> (FieldP,FieldP)
add (xn,zn) (xm,zm) (xd,zd) = let x = 4 * (xm * xn - zm * zn)^2 * zd
                                  z = 4 * (xm * zn - zm * xn)^2 * xd
                               in (x,z)

double :: (FieldP,FieldP) -> (FieldP,FieldP)
double (xn,zn) = let x = (xn^2 - zn^2)^2
                     z = 4 * xn * zn * (xn^2 + a * xn * zn + zn^2)
                  in (x,z)

curve25519 :: Integer -> FieldP -> FieldP
curve25519 n base = let one = (base,1)
                        two = double one
                        ((x,z),_) = f n
                        f :: Integer -> ((FieldP,FieldP),(FieldP,FieldP))
                        f m = if m == 1
                              then (one,two)
                              else let (pm,pm1) = f (m `div` 2) in
                                  if odd m
                                  then (add pm pm1 one, double pm1)
                                  else (double pm, add pm pm1 one)
                    in x/z

unpackI :: B.ByteString -> Integer
unpackI s = if B.length s /= 32
            then error "Invalid Curve25519 argument"
            else sum $ zipWith (*) (map toInteger $ B.unpack s) [256^i|i<-[0..31]]

packI :: Integer -> B.ByteString
packI n = if n < 0 || n >= 2^256
          then error "Invalid Curve25519 argument"
          else B.pack $ map (\i -> fromInteger (n `div` (256^i))) [0..31]

clamp :: B.ByteString -> B.ByteString
clamp bs = let s = B.unpack bs
               h = head s
               l = last s
               s' = [h - h `mod` 8] ++ (take 30 $ drop 1 s) ++ [64 + l `mod` 64]
            in B.pack s'

dh :: B.ByteString -> B.ByteString -> B.ByteString
dh sk pk = let FieldP x = curve25519 (unpackI $ clamp sk) (fromInteger $ unpackI pk)
            in packI x

unsecret :: B.ByteString -> B.ByteString
unsecret sk = dh sk (packI 9)