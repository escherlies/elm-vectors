module Vector3 exposing (..)

import Expect
import Fuzz
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Fun"
        [ fuzz fuzzVecFloat
            "getX"
            (\{ g, a } -> equalFloat g.x (Math.Vector3.getX a))
        , fuzz fuzzVecFloat
            "getY"
            (\{ g, a } -> equalFloat g.y (Math.Vector3.getY a))
        , fuzz fuzzVecFloat
            "setX"
            (\{ g, a } -> Expect.equal (vec3 8 g.y g.z) (Math.Vector3.setX 8 a))
        , fuzz fuzzVecFloat
            "setY"
            (\{ g, a } -> Expect.equal (vec3 g.x 8 g.z) (Math.Vector3.setY 8 a))
        , fuzz fuzzVecFloat
            "add"
            (\{ g, a } -> Expect.equal (vec3 (g.x * 2) (g.y * 2) (g.z * 2)) (Math.Vector3.add a a))
        , fuzz fuzzVecFloat
            "sub"
            (\{ a } -> Expect.equal (vec3 0 0 0) (Math.Vector3.sub a a))
        , fuzz fuzzVecFloat
            "negate"
            (\{ g, a } -> Expect.equal (vec3 -g.x -g.y -g.z) (Math.Vector3.negate a))
        , fuzz fuzzVecFloat
            "scale"
            (\{ g, a } -> Expect.equal (vec3 (8 * g.x) (8 * g.y) (8 * g.z)) (Math.Vector3.scale 8 a))
        , fuzz fuzzVecFloat
            "dot"
            (\_ -> Expect.equal 32 (Math.Vector3.dot (vec3 1 2 3) (vec3 4 5 6)))
        , fuzz fuzzVecFloat
            "normalize"
            (\_ -> Expect.equal (vec3 0.5773502691896258 0.5773502691896258 0.5773502691896258) (Math.Vector3.normalize (vec3 1 1 1)))
        , fuzz fuzzVecFloat
            "direction"
            (\_ -> Expect.equal (vec3 0 -1 0) (Math.Vector3.direction (vec3 1 2 0) (vec3 1 3 0)))
        , fuzz fuzzVecFloat
            "length"
            (\_ -> equalFloat (sqrt 14) (Math.Vector3.length (vec3 1 2 3)))
        , fuzz fuzzVecFloat
            "lengthSquared"
            (\_ -> Expect.equal 1 (Math.Vector3.lengthSquared (vec3 1 0 0)))
        , fuzz fuzzVecFloat
            "distance"
            (\_ -> equalFloat (2 * sqrt 2) (Math.Vector3.distance (vec3 1 2 0) (vec3 3 4 0)))
        , fuzz fuzzVecFloat
            "distanceSquared"
            (\_ -> Expect.equal 1 (Math.Vector3.distanceSquared (vec3 1 0 0) (vec3 0 0 0)))
        , fuzz fuzzVecFloat
            "toRecord"
            (\{ g, a } -> Expect.equal g (Math.Vector3.toRecord a))
        , fuzz fuzzVecFloat
            "fromRecord"
            (\{ g, a } -> Expect.equal a (Math.Vector3.fromRecord g))
        , fuzz fuzzVecFloat
            "divBy"
            (\_ -> Expect.equal (vec3 1 2 3) (Math.Vector3.divBy 10 (vec3 10 20 30)))
        , fuzz fuzzVecFloat
            "map"
            (\{ a } -> Expect.equal a (Math.Vector3.map identity a))
        , fuzz fuzzVecFloat
            "mapY"
            (\{ a } -> Expect.equal a (Math.Vector3.mapY identity a))
        , fuzz fuzzVecFloat
            "mul"
            (\{ a } -> Expect.equal a (Math.Vector3.mul a (vec3 1 1 1)))
        , fuzz fuzzVecInt
            "toFloat"
            (\{ g, a } -> Expect.equal (vec3 (Basics.toFloat g.x) (Basics.toFloat g.y) (Basics.toFloat g.z)) (Math.Vector3.toFloat a))
        ]



-- Util


equalFloat : Float -> Float -> Expect.Expectation
equalFloat =
    Expect.within (Expect.Absolute 1.0e-12)


fuzzVec : Fuzz.Fuzzer c -> Fuzz.Fuzzer c -> Fuzz.Fuzzer c -> Fuzz.Fuzzer { g : { x : c, y : c, z : c }, a : Math.Vector3.Vec c }
fuzzVec =
    Fuzz.map3
        (\x y z ->
            { g = { x = x, y = y, z = z }
            , a = vec3 x y z
            }
        )


fuzzVecInt : Fuzz.Fuzzer { g : { x : Int, y : Int, z : Int }, a : Math.Vector3.Vec Int }
fuzzVecInt =
    fuzzVec
        Fuzz.int
        Fuzz.int
        Fuzz.int


fuzzVecFloat : Fuzz.Fuzzer { g : { x : Float, y : Float, z : Float }, a : Math.Vector3.Vec Float }
fuzzVecFloat =
    fuzzVec
        Fuzz.niceFloat
        Fuzz.niceFloat
        Fuzz.niceFloat
