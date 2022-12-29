module Vector2 exposing (..)

import Expect
import Fuzz
import Math.Vector2 exposing (vec2)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Fun"
        [ fuzz fuzzVecFloat
            "getX"
            (\{ g, a } -> equalFloat g.x (Math.Vector2.getX a))
        , fuzz fuzzVecFloat
            "getY"
            (\{ g, a } -> equalFloat g.y (Math.Vector2.getY a))
        , fuzz fuzzVecFloat
            "setX"
            (\{ g, a } -> Expect.equal (vec2 8 g.y) (Math.Vector2.setX 8 a))
        , fuzz fuzzVecFloat
            "setY"
            (\{ g, a } -> Expect.equal (vec2 g.x 8) (Math.Vector2.setY 8 a))
        , fuzz fuzzVecFloat
            "add"
            (\{ g, a } -> Expect.equal (vec2 (g.x * 2) (g.y * 2)) (Math.Vector2.add a a))
        , fuzz fuzzVecFloat
            "sub"
            (\{ a } -> Expect.equal (vec2 0 0) (Math.Vector2.sub a a))
        , fuzz fuzzVecFloat
            "negate"
            (\{ g, a } -> Expect.equal (vec2 -g.x -g.y) (Math.Vector2.negate a))
        , fuzz fuzzVecFloat
            "scale"
            (\{ g, a } -> Expect.equal (vec2 (8 * g.x) (8 * g.y)) (Math.Vector2.scale 8 a))
        , fuzz fuzzVecFloat
            "dot"
            (\_ -> Expect.equal 11 (Math.Vector2.dot (vec2 1 2) (vec2 3 4)))
        , fuzz fuzzVecFloat
            "normalize"
            (\_ -> Expect.equal (vec2 0.7071067811865475 0.7071067811865475) (Math.Vector2.normalize (vec2 1 1)))
        , fuzz fuzzVecFloat
            "direction"
            (\_ -> Expect.equal (vec2 0 -1) (Math.Vector2.direction (vec2 1 2) (vec2 1 3)))
        , fuzz fuzzVecFloat
            "length"
            (\_ -> equalFloat (sqrt 5) (Math.Vector2.length (vec2 1 2)))
        , fuzz fuzzVecFloat
            "lengthSquared"
            (\_ -> Expect.equal 1 (Math.Vector2.lengthSquared (vec2 1 0)))
        , fuzz fuzzVecFloat
            "distance"
            (\_ -> equalFloat (2 * sqrt 2) (Math.Vector2.distance (vec2 1 2) (vec2 3 4)))
        , fuzz fuzzVecFloat
            "distanceSquared"
            (\_ -> Expect.equal 1 (Math.Vector2.distanceSquared (vec2 1 0) (vec2 0 0)))
        , fuzz fuzzVecFloat
            "toRecord"
            (\{ g, a } -> Expect.equal g (Math.Vector2.toRecord a))
        , fuzz fuzzVecFloat
            "fromRecord"
            (\{ g, a } -> Expect.equal a (Math.Vector2.fromRecord g))
        , fuzz fuzzVecFloat
            "divBy"
            (\_ -> Expect.equal (vec2 1 2) (Math.Vector2.divBy 10 (vec2 10 20)))
        , fuzz fuzzVecFloat
            "map"
            (\{ a } -> Expect.equal a (Math.Vector2.map identity a))
        , fuzz fuzzVecFloat
            "mapY"
            (\{ a } -> Expect.equal a (Math.Vector2.mapY identity a))
        , fuzz fuzzVecFloat
            "mul"
            (\{ a } -> Expect.equal a (Math.Vector2.mul a (vec2 1 1)))
        , fuzz fuzzVecInt
            "toFloat"
            (\{ g, a } -> Expect.equal (vec2 (Basics.toFloat g.x) (Basics.toFloat g.y)) (Math.Vector2.toFloat a))
        ]



-- Util


equalFloat : Float -> Float -> Expect.Expectation
equalFloat =
    Expect.within (Expect.Absolute 1.0e-12)


fuzzVec : Fuzz.Fuzzer b -> Fuzz.Fuzzer b -> Fuzz.Fuzzer { g : { x : b, y : b }, a : Math.Vector2.Vec b }
fuzzVec =
    Fuzz.map2
        (\x y ->
            { g = { x = x, y = y }
            , a = vec2 x y
            }
        )


fuzzVecInt : Fuzz.Fuzzer { g : { x : Int, y : Int }, a : Math.Vector2.Vec Int }
fuzzVecInt =
    fuzzVec Fuzz.int Fuzz.int


fuzzVecFloat : Fuzz.Fuzzer { g : { x : Float, y : Float }, a : Math.Vector2.Vec Float }
fuzzVecFloat =
    fuzzVec Fuzz.niceFloat Fuzz.niceFloat
