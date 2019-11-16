module Main where

import Lib
import Quiver

main :: IO ()
main = do
    let [v1,v2,v3,v4] = [Vertex "1", Vertex "2", Vertex "3", Vertex "4"]
    let [a1,a2,a3] = [Arrow "a" v1 v1, Arrow "b" v2 v3, Arrow "c" v3 v4]
    let q = Quiver "Q" [v1] [a1]
    let p = Path "aaaa" [a1,a1,a1,a1]
    print (getAllPaths q)