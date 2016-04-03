module Test.FRP.Netwire.Samples.YampaRedefs where

{--------------------------------------------------------------------
    Redefinitions of types from Yampa
--------------------------------------------------------------------}

data Vector2D = V2 Float Float
    deriving (Eq, Show)

(^+^) :: Vector2D -> Vector2D -> Vector2D
(V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

(^-^) :: Vector2D -> Vector2D -> Vector2D
(V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)

(^*) :: Vector2D -> Float -> Vector2D
(V2 x y) ^* val = V2 (x * val) (y * val)

distance :: Vector2D -> Vector2D -> Float
distance (V2 x1 y1) (V2 x2 y2) = sqrt (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))

magnitude :: Vector2D -> Float
magnitude (V2 x y) = sqrt ((x * x) + (y * y))

normalize :: Vector2D -> Vector2D
normalize vec@(V2 x y) = V2 (x / len) (y / len)
    where len = magnitude vec

zero :: Vector2D
zero = V2 0 0

instance Num Vector2D where
    (+) = (^+^)
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
    (-) = (^-^)
    negate (V2 x y) = V2 (negate x) (negate y)
    abs (V2 x y) = undefined
    signum (V2 x y) = undefined
    fromInteger int = V2 (fromInteger int) (fromInteger int)

instance Fractional Vector2D where
    (V2 x1 y1) / (V2 x2 y2) = undefined
    recip (V2 x y) = undefined
    fromRational rat = V2 (fromRational rat) (fromRational rat)
