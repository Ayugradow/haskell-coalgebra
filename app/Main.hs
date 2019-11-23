module Main where

import Lib
import Quiver
import Control.Monad

main :: IO ()
main = do
    let [v1,v2,v3,v4,v5,v6] = [Vertex "1", Vertex "2", Vertex "3", Vertex "4", Vertex "5", Vertex "6"]
    let a = Arrow "a" v1 v2
    let b = Arrow "b" v2 v3
    let c = Arrow "c" v3 v4
    let d = Arrow "d" v4 v5
    let e = Arrow "e" v5 v6
    let f = Arrow "f" v6 v5
    let g = Arrow "g" v5 v4
    let h = Arrow "h" v4 v3
    let i = Arrow "i" v3 v2
    let j = Arrow "j" v2 v1
    let quiv = Quiver "Q" [v1,v2,v3,v4,v5,v6] [a,b,c,d,e,f,g,h,i,j]
    let e1e5quive1e5 = localQuiver [v1,v5] quiv
    print (localQuiver [v1,v5] quiv)