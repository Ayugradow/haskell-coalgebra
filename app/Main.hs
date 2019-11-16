module Main where

import Lib
import Quiver

main :: IO ()
main = do
    let [v1,v2,v3,v4] = [Vertex "1", Vertex "2", Vertex "3", Vertex "4"]
    let [a1,a2,a3] = [Arrow "a" v1 v2, Arrow "b" v2 v3, Arrow "c" v3 v4]
    let a4 = Arrow "d" v1 v1
    let q = Quiver "Q" [v1,v2] [a1]
    let q1 = Quiver "Q1" [v1,v2,v3,v4] [a1,a2,a3]
    let q2 = Quiver "Q2" [v1] [a4]
    print (getAllPaths q)
    print (getAllPaths q1)
    print (getAllPaths q2)
    print ((%) (Path "p" [a1,a2,a3]))
    print ((%) (Path "p1" [a1]))
    print ((%) (Path "p2" [a4]))
    print ((%) (stationaryPath v1))
    print ((%) (arrowPath a1))