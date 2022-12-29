# elm-vectors

Basic linear algebra operations for working with vectors in Elm.

If you do not work with WebGl, you can use this as a much faster alternative than the [elm-explorations/linear-algebra](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/) package.
See [Benchmarks](#Benchmarks) for some performance comparisons. You can use this package as a drop-in replacement of the beforementionend.

# Usage

```elm
a : Vec Float
a =
    vec2 1 2


doSomeMath : Vec Float -> Vec Float
doSomeMath b =
    b
        |> sub a
        |> add (vec2 3 4)
        |> normalize
        |> scale 5
```

You can work with integer vectors as well, although most calculations require floats.

In that case it useful to convert it to an integer vector at the last step:

```elm
doSomeIntMath : Vec Int -> Vec Int
doSomeIntMath =
    Math.Vector2.round << doSomeMath << Math.Vector2.toFloat
```

# Other packages

- [elm-explorations/linear-algebra](https://package.elm-lang.org/packages/elm-explorations/linear-algebra/latest/): Way slower, but required for Elm-WebGl.
- [justgook/alt-linear-algebra](https://package.elm-lang.org/packages/justgook/alt-linear-algebra/latest/): Uses slower records and only works with floats.
- [jjant/linear-algebra](https://package.elm-lang.org/packages/jjant/linear-algebra/latest/): Uses slower records and only works with floats.

# Benchmarks

See [escherlies/elm-vector-impl-perf](https://github.com/escherlies/elm-vector-impl-perf)

