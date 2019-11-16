module Main where

import Lib
import Quiver

main :: IO ()
main = do
    let [v1,v2,v3,v4,v5] = [Vertex "1", Vertex "2", Vertex "3", Vertex "4", Vertex "5"]
    let [a1,a2,a3,a4,a5] = [Arrow "a" v1 v2, Arrow "b" v2 v4, Arrow "c" v1 v3, Arrow "d" v3 v4, Arrow "e" v3 v5]
    let q = Quiver "Q" [v1,v2,v3,v4,v5] [a1,a2,a3,a4,a5]
    let paths = getAllPaths q
    print paths
    print [pathLength p | p <- paths]
