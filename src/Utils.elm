module Utils exposing (..)


b1 : (b -> c) -> (a -> d -> b) -> a -> d -> c
b1 =
    (<<) << (<<)
