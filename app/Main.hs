module Main where

import Data.Maybe
import Lib
import Quiver

main :: IO ()
main = do
    let [v1,v2,v3] = [Vertex "1", Vertex "2", Vertex "3"]
    let [a1,a2] = [Arrow "a" v1 v2, Arrow "b" v2 v1]
    let q = Quiver "Q" [v1,v2,v3] [a1,a2]
    print (getAllPaths q)
    print (hasCycles q)