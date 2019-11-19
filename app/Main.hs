module Main where

import Lib
import Quiver
import Control.Monad

main :: IO ()
main = do
    let [v1,v2,v3,v4,v5,v6] = [Vertex "1", Vertex "2", Vertex "3", Vertex "4", Vertex "5", Vertex "6"]
    let a = Arrow "a" v1 v2
    let b = Arrow "b" v3 v2
    let c = Arrow "c" v4 v1
    let d = Arrow "d" v2 v4
    let e = Arrow "e" v4 v3
    let f = Arrow "f" v4 v5
    let g = Arrow "g" v4 v5
    let h = Arrow "h" v1 v5
    let i = Arrow "i" v5 v2
    let j = Arrow "j" v3 v5
    let k = Arrow "k" v1 v1
    let l = Arrow "l" v2 v2
    let m = Arrow "m" v3 v3
    let n = Arrow "n" v4 v4
    let o = Arrow "o" v5 v5
    let p = Arrow "p" v6 v6
    let q = Arrow "q" v6 v6
    let quiv = Quiver "Q" [v1,v2,v3,v4,v5] [a,b,c,d,e,f,g,h,i,j]
    let e' = [v1,v2,v3]
    print (map (#v1) (pathsFrom v1 quiv))
    print (pathsFromTo v1 v1 quiv)
    print (pathsToFrom v1 v1 quiv)