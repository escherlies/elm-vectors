module Math.Vector2 exposing
    ( Vec(..), vec2
    , getX, getY, setX, setY
    , add, sub, negate, scale, divBy, dot, mul, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , mapX, mapY, map
    , apply, lift2, lift3
    , toRecord, fromRecord
    , toFloat, round, floor, ceiling, truncate
    )

{-| A high performance linear algebra library using native Elm ADT.


# Create

@docs Vec, vec2


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY


# Operations

@docs add, sub, negate, scale, divBy, dot, mul, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Transform

@docs mapX, mapY, map
@docs apply, lift2, lift3


# Conversions

@docs toRecord, fromRecord
@docs toFloat, round, floor, ceiling, truncate

-}


{-| Two dimensional vector type
-}
type Vec a
    = Vec a a


{-| Creates a new 2-element vector with the given values.
-}
vec2 : a -> a -> Vec a
vec2 =
    Vec


{-| Extract the x component of a vector.
-}
getX : Vec a -> a
getX (Vec x _) =
    x


{-| Extract the y component of a vector.
-}
getY : Vec a -> a
getY (Vec _ y) =
    y


{-| Update the x component of a vector, returning a new vector.
-}
setX : a -> Vec a -> Vec a
setX x (Vec _ y) =
    Vec x y


{-| Update the y component of a vector, returning a new vector.
-}
setY : a -> Vec a -> Vec a
setY y (Vec x _) =
    Vec x y


{-| Convert a vector to a record.
-}
toRecord : Vec a -> { x : a, y : a }
toRecord (Vec x y) =
    { x = x, y = y }


{-| Convert a record to a vector.
-}
fromRecord : { x : a, y : a } -> Vec a
fromRecord { x, y } =
    Vec x y


{-| Vector addition: a + b
-}
add : Vec number -> Vec number -> Vec number
add (Vec x1 y1) (Vec x2 y2) =
    Vec (x1 + x2) (y1 + y2)


{-| Vector subtraction: a - b
-}
sub : Vec number -> Vec number -> Vec number
sub (Vec x1 y1) (Vec x2 y2) =
    Vec (x1 - x2) (y1 - y2)


{-| Vector negation: -a
-}
negate : Vec number -> Vec number
negate (Vec x y) =
    Vec -x -y


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec Float -> Vec Float -> Vec Float
direction a b =
    let
        ab =
            sub a b
    in
    divBy (length ab) ab


{-| The length of the given vector: |a|
-}
length : Vec Float -> Float
length (Vec x y) =
    sqrt (x ^ 2 + y ^ 2)


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec number -> number
lengthSquared (Vec x y) =
    x ^ 2 + y ^ 2


{-| The distance between two vectors.
-}
distance : Vec Float -> Vec Float -> Float
distance a b =
    length (sub a b)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec number -> Vec number -> number
distanceSquared a b =
    lengthSquared (sub a b)


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec Float -> Vec Float
normalize a =
    divBy (length a) a


{-| Multiply the vector by a scalar: s \* v
-}
scale : number -> Vec number -> Vec number
scale k (Vec x y) =
    Vec (k * x) (k * y)


{-| Divide the vector by a scalar: v / s

Shorthand for `scale (1 / s)`

-}
divBy : Float -> Vec Float -> Vec Float
divBy k (Vec x y) =
    Vec (x / k) (y / k)


{-| The dot product of a and b
-}
dot : Vec number -> Vec number -> number
dot (Vec x1 y1) (Vec x2 y2) =
    (x1 * x2) + (y1 * y2)


{-| Component-wise multiplication
-}
mul : Vec number -> Vec number -> Vec number
mul (Vec x1 y1) (Vec x2 y2) =
    Vec (x1 * x2) (y1 * y2)



-- Transform


{-| Map a fn to the x value
-}
mapX : (a -> a) -> Vec a -> Vec a
mapX fn (Vec x y) =
    Vec (fn x) y


{-| Map a fn to the y value
-}
mapY : (a -> a) -> Vec a -> Vec a
mapY fn (Vec x y) =
    Vec x (fn y)


{-| Map a fn to the x and y value
-}
map : (a -> b) -> Vec a -> Vec b
map fn (Vec x y) =
    Vec (fn x) (fn y)


{-| -}
apply : Vec (a -> b) -> Vec a -> Vec b
apply (Vec fnx fny) (Vec x y) =
    Vec (fnx x) (fny y)


{-| Lift a binary function to vectors.

    lift2 (+) (Vec 1 1) (Vec 2 2)
    --> Vec 3 3 : Vec number

Keep in mind that this comes at a cost of doing a step extra and if you need performance,
you might be better off deconstructing and applying the function manually.

-}
lift2 : (a -> b -> c) -> Vec a -> Vec b -> Vec c
lift2 fn (Vec x1 y1) (Vec x2 y2) =
    Vec (fn x1 x2) (fn y1 y2)


{-| -}
lift3 : (a -> b -> c -> d) -> Vec a -> Vec b -> Vec c -> Vec d
lift3 fn (Vec x1 y1) (Vec x2 y2) (Vec x3 y3) =
    Vec (fn x1 x2 x3) (fn y1 y2 y3)



-- Convert


{-| Convert an integer vector into a float vector.
-}
toFloat : Vec Int -> Vec Float
toFloat =
    map Basics.toFloat


{-| Round the number components to the nearest integer.
-}
round : Vec Float -> Vec Int
round =
    map Basics.round


{-| Floor function, rounding down.
-}
floor : Vec Float -> Vec Int
floor =
    map Basics.floor


{-| Ceiling function, rounding up.
-}
ceiling : Vec Float -> Vec Int
ceiling =
    map Basics.ceiling


{-| Truncate the number components, rounding towards zero.
-}
truncate : Vec Float -> Vec Int
truncate =
    map Basics.truncate
