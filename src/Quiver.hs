{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DatatypeContexts  #-}

module Quiver (
    Quiver (..)
    , Vertex(..)
    , Arrow(..)
    , Chain(..)
    , Path(..)
    , Tensor(..)
    , (⊗)
    , (#)
    , (%)
    , (.+.)
    , (.-.)
    , (-.)
    , (.*)
    -- , (+⊗+)
    , emptyPath
    , maxPathLength
    , hasCycles
    , longComp
    , stationaryPath
    , arrowPath
    , paths
    , pathsFrom
    , pathsTo
    , arrowsFrom
    , arrowsTo
    , tailsFrom
    , cellsFrom
    , headsTo
    , pathHeads
    , pathTails
    , isHead
    , isTail
    , localQuiver
    , mapPair
    ) where
        import Data.List
        import Data.Function
        import Data.AdditiveGroup

    -- Begin Exported

        -- Vertices only have names
        data Vertex = Vertex {
            vertexName :: String
        }

        -- Arrows have names, source and target
        data Arrow = Arrow {
            arrowName :: String,
            arrowSource :: Vertex,
            arrowTarget :: Vertex
        }

        -- Quivers are a collection of vertices and arrows connecting them
        data Quiver = Quiver {
            quiverName :: String,
            vertices :: [Vertex],
            arrows :: [Arrow]
        }

        -- Paths are collections of composable arrows
        data Chain = Chain {
            pathName :: String,
            pathArrows :: [Arrow],
            pathSource :: Vertex,
            pathTarget :: Vertex
        }

        data Tensor a b = Tensor {
            leftT :: [a],
            rightT :: [b]
        }

        data Mod a b = Mod {
            scalar :: a,
            vec :: b
        }

        -- Actual definition of how to compose two composable types
        (#) :: (Path a, Path b) => a->b->Chain
        (#) a b = (path a)<+>(path b)

        -- The inverse operation to composing
        -- Breaks a path into all its possible subpaths
        (%) :: (Path a, Num b, Eq b) => a -> [(b,[(Chain,Chain)])]
        (%) p = [ (1, [x]) | x<-(map filterDeltaEmptyPaths . splitPath) p]

        (.+.) :: (Num a, Ord a, Show a, Path b) => [(a,b)] -> [(a,b)] -> [(a,[Chain])]
        (.+.) xs ys = [ (fst x, [path . snd $ x]) | x<-xs ] ^+^ [ (fst y, [path . snd $ y]) | y<-ys ]

        -- (.-.) :: (Num a, Ord a, Show a, Path b) => [(a,b)] -> [(a,b)] -> [(a,[Chain])]
        -- (.-.) xs ys = (.+.) xs negateV( [ (fst y, path . snd $ y) | y<-ys] )

        (-.) :: (Num a, Ord a, AdditiveGroup b) => [(a,b)] -> [(a,b)]
        (-.) ps = [ (-fst p, snd p) | p<-ps ]

        (.*) :: (Num a, Ord a, AdditiveGroup b) => a -> [(a,b)] -> [(a,b)]
        n.*ps = [ (n * fst p, snd p) | p<-ps ]

        -- Gets the maximum size of a path in a quiver
        maxPathLength :: Quiver -> Int
        maxPathLength (Quiver n vs []) = 0
        maxPathLength (Quiver _ _ as) = length as

        
        paths :: Quiver -> [Chain]
        paths q = sort ([ stationaryPath v | v <- vertices q] ++ concat [ pathsFrom v q | v<-vertices q])

        -- Checks if a quiver has cycles
        -- This works because in a set with "n" symbols, the maximal word with distinc symbols has precisely all the symbols
        -- So if a path has more than maxPathLegth arrows, we know it's a cycle
        hasCycles :: Quiver -> Bool
        hasCycles (Quiver n vs []) = False
        hasCycles q = filterEmptyPaths (getPathsFromWords (succ (maxPathLength q)) q) /= []

        localQuiver :: (Path a) => [a] -> Quiver -> Quiver
        localQuiver as q = Quiver ((concatMap (pathName . path) as) ++ (quiverName q) ++ (concatMap (pathName . path) as)) (map toVertex as) (map toArrow (cellsFrom as q))

    -- End Exported

    -- Begin Auxiliary

        -- "Path" class of things that are path-like
        class Path a where
            source      :: a -> Vertex
            target      :: a -> Vertex
            path        :: a -> Chain
            pathLength  :: a -> Int



        -- Redefining the show for each type
        -- Can be easily rewritten to show different things
        instance Show Vertex where
            show (Vertex s) = s

        -- Show arrow is done by literally drawing the arrow between its source and target vertices
        -- so we need some string manipulation
        instance Show Arrow where
            -- uncomment this to allow verbose shows - i.e. showing 1 -a-> 2 instead of just a 
            -- show (Arrow a s t) = show s ++ " -" ++ a ++ "-> " ++ show t
            show = arrowName

        instance Show Chain where
            -- uncomment this to allow verbose shows - i.e. showing 1 -a-> 2 -b-> 3 instead of just ab
            -- show (Chain "" [] s t) = "Empty path"
            -- show (Chain n as s t) = n ++ ": " ++ pathJoin as
            show = pathName

        -- Sadly we cannot draw the quivers as they will, more often than not, be non-planar
        instance Show Quiver where
            show (Quiver s v a) = "Quiver " ++ s ++ ": {Vertices: " ++ (strJoin ", " (map show v)) ++ ". Arrows: " ++ (strJoin ", " (map show a)) ++ "}"
        


        -- instance Show (Tensor Chain Chain) where
        --     show = show . toTuple

        instance (Show a) => Show (Tensor a a) where
            show = show . toTuple

        instance (Num a, Ord a, Show a, AdditiveGroup b, Show b) =>  Show (Mod a b) where
            show (Mod n x) = show (n,x)

        -- Redefining equality for our types
        instance Eq Vertex where
            (Vertex s) == (Vertex t) = s == t

        instance Eq Arrow where
            (Arrow n s t) == (Arrow m d y) = n == m && s == d && t == y

        instance Eq Chain where
            (Chain n as s t) == (Chain m bs s' t')  = n == m

        instance Eq Quiver where
            (Quiver n vs as) == (Quiver m ws bs) = n == m && vs == ws && as == bs
        
        -- instance Eq Tensor where
        --     x == y = and [leftT x == leftT y, rightT x == rightT y]

        instance Ord Chain where
            (<) c d | length (pathArrows c) == length (pathArrows d) = (<) (pathName c) (pathName d)
                    | otherwise = (<) (length (pathArrows c)) (length (pathArrows d))

            (>) c d | length (pathArrows c) == length (pathArrows d) = (>) (pathName c) (pathName d)
                    | otherwise = (>) (length (pathArrows c)) (length (pathArrows d))

            (<=) c d | length (pathArrows c) == length (pathArrows d) = (<=) (pathName c) (pathName d)
                    | otherwise = (<=) (length (pathArrows c)) (length (pathArrows d))

            (>=) c d | length (pathArrows c) == length (pathArrows d) = (>=) (pathName c) (pathName d)
                    | otherwise = (>=) (length (pathArrows c)) (length (pathArrows d))

            max c d | length (pathArrows c) == length (pathArrows d) =
                        if max (pathName c) (pathName d) == pathName c
                            then c
                            else d
                    | otherwise = 
                        if max (length (pathArrows c)) (length (pathArrows d)) == length (pathArrows c)
                            then c
                            else d

            min c d | length (pathArrows c) == length (pathArrows d) =
                        if min (pathName c) (pathName d) == pathName c
                            then c
                            else d
                    | otherwise = 
                        if min (length (pathArrows c)) (length (pathArrows d)) == length (pathArrows c)
                            then c
                            else d

        instance (Eq a, Eq b) =>  Eq (Tensor a b) where
            (==) x y = (toTuple x) == (toTuple y)

        instance (Eq a, Eq b) => Eq (Mod a b) where
            (==) (Mod n x) (Mod m y) = (n,x) == (m,y)

        instance (Ord a, Ord b) =>  Ord (Tensor a b) where
            (<)     x y = (toTuple x)   <   (toTuple y)
            (<=)    x y = (toTuple x)   <=  (toTuple y)
            (>)     x y = (toTuple x)   >   (toTuple y)
            (>=)    x y = (toTuple x)   >=  (toTuple y)

        instance (Ord a, Ord b) =>  Ord (Mod a b) where
            (<)     (Mod n x) (Mod m y) = (n,x)   <   (m,y)
            (<=)    (Mod n x) (Mod m y) = (n,x)   <=  (m,y)
            (>)     (Mod n x) (Mod m y) = (n,x)   >   (m,y)
            (>=)    (Mod n x) (Mod m y) = (n,x)   >=  (m,y)
                            
        -- Defining how to compose each path type
        instance Path Chain where
            source = pathSource
            target = pathTarget
            path = id
            pathLength = length . pathArrows  

        instance Path Vertex where
            source = id
            target = id
            path = stationaryPath
            pathLength = const 0

        instance Path Arrow where
            source = arrowSource
            target = arrowTarget
            path = arrowPath
            pathLength = const 1

        -- instance AdditiveGroup [Chain] where
        --     zeroV           = [emptyPath]
        --     (^+^)   xs  ys  = sort(xs ++ ys)
        --     negateV xs      = map invertChain xs

        instance (Path a) => AdditiveGroup [a] where
            zeroV = []
            (^+^)   xs  ys  = sort(xs ++ ys)

        instance (Num a, Show a, Ord a, AdditiveGroup b, Ord b) =>AdditiveGroup [(a,b)] where
            zeroV = [(0,zeroV)]
            (^+^) xs ys = showSum(xs ++ ys)
            negateV xs = [ (- fst x, snd x ) | x<-xs ]

        instance (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => AdditiveGroup [Tensor a b] where
            zeroV           = [Tensor [zeroV] [zeroV] ]
            (^+^)   xs  ys  = toTensor . showSumT  $ (concatMap toTuple xs) ++ (concatMap toTuple ys)
            negateV xs      = [ Tensor (map negateV (leftT x)) (rightT x) | x<-xs]

        (⊗) :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => a -> b -> Tensor a b
        x⊗y = Tensor [x] [y]


        isComposable :: Chain->Chain->Bool
        isComposable x y = target x == source y

        doComp :: Chain->Chain->Chain
        doComp x y  | and [source x == target x, pathArrows x == []] = y
                    | and [source y == target y, pathArrows y == []] = x
                    | otherwise = Chain (pathName x ++ pathName y) (pathArrows x ++ pathArrows y) (source x) (target y)

        -- Basic path composition
        (<+>) :: Chain->Chain->Chain
        x<+>y   | isComposable x y = doComp x y
                | otherwise = emptyPath

        (<**>) :: (Num a, Show a, Ord a) => a -> Chain -> Chain
        n<**>x  | n == 0 = emptyPath
                | n == 1 = x
                | n == -1 = invertChain x
                | otherwise = Chain (show n ++ pathName x) (pathArrows x) (source x) (target x)

        -- (+⊗+) :: (Num a, Show a, Ord a) => (a,Tensor) -> (a,Tensor) -> [(a,Tensor)]
        -- x+⊗+y  | x == y = [(fst x + fst y, snd x)]
        --         | (leftT . snd $ x) == (leftT . snd $ y) = [(fst x,(leftT . snd $ x)⊗((rightT . snd $ x) ++ (rightT . snd $ y)))]
        --         | (rightT . snd $ x) == (rightT . snd $ y) = [(fst x,((leftT . snd $ x) ++ (leftT . snd $ y))⊗(rightT . snd $ x))]
        --         | otherwise = [x,y]

        -- Maps a pair of maps "f :: a->c" and "g :: b->d" to a pair "(a,b)" to obtain a pair "(c,d)"
        mapPair :: (a-> c)->(b-> d)->(a,b)->(c, d)
        mapPair f g (x,y) = (f x, g y)

        mapPair' :: (a-> c)->(a-> c)->[a]->[c]
        mapPair' f g [x,y] = [ f x ] ++ [ g y ]

        crossPairs :: [a]->[a]->[[a]]
        crossPairs x [] = [x]
        crossPairs [] y = [y]
        crossPairs [x] ys = [ [x]++[y] | y<-ys ]
        crossPairs xs [y] = [ [x]++[y] | x<-xs ]
        crossPairs (x:xs) ys = crossPairs [x] ys ++ crossPairs xs ys

        toTuple :: Tensor a b -> [(a,b)]
        toTuple (Tensor xs ys) = [ (x,y) | x<-xs, y<-ys]

        toTensor :: [(a,b)] -> [Tensor a b]
        toTensor as = [ Tensor [fst a] [snd a] | a<-as ]

        isNegativeV :: Chain -> Bool
        isNegativeV x = Chain ( stripChars "-" (pathName x)) (pathArrows x) (source x) (target x) /= x

        -- isNegativeT :: ([Chain],[Chain]) -> Bool
        -- isNegativeT x = and [ isNegativeV l | l<-leftT x] /= and [ isNegativeV r | r<-rightT x]

        invertChain :: Chain -> Chain
        invertChain x   | isNegativeV x = Chain ((tail . pathName) x) (pathArrows x) (source x) (target x)
                        | otherwise     = Chain ("-" ++ pathName x) (pathArrows x) (source x) (target x)

        -- invertTensor :: Tensor -> Tensor
        -- invertTensor x  | and [isNegativeT x, and [isNegativeV r | r<-rightT x]]    = Tensor (leftT x) [invertChain r | r<-rightT x]
        --                 | otherwise                                                 = Tensor [invertChain l | l<-leftT x] (rightT $ x)

        combineSums :: (Num a, Ord a, Ord b, AdditiveGroup b) => [(a,b)] -> [[(a,b)]]
        combineSums xs = (groupBy ((==) `on` snd)) . sort $ xs

        simplifySums :: (Num a, Ord a, Ord b, AdditiveGroup b) => [[(a,b)]] -> [(a,b)]
        simplifySums as = [ (foldl1 (+) (map fst a), snd x) | a<-as, x<-a]

        filterSums :: (Num a, Ord a, Ord b, AdditiveGroup b) => [(a,b)] -> [(a,b)]
        filterSums xs = [ (fst x, snd x) | x<-[ head p | p<-group . sort $ xs ], fst x /= 0 ]

        showSum :: (Num a, Ord a, Ord b, AdditiveGroup b) => [(a,b)] -> [(a,b)]
        showSum = filterSums . simplifySums . combineSums

        gatherSumsT1 :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [(a,b)] -> [[(a,b)]]
        gatherSumsT1 xs = (groupBy ((==) `on` fst)) . sort $ xs

        combineSumsT1 :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [[(a,b)]] -> [(a,b)]
        combineSumsT1 xs = [ (fst . head $ l, foldl1 (^+^) (map snd l)) | l<-xs ]

        gatherSumsT2 :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [(a,b)] -> [[(a,b)]]
        gatherSumsT2 xs = (groupBy ((==) `on` snd)) . sort $ xs

        combineSumsT2 :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [[(a,b)]] -> [(a,b)]
        combineSumsT2 xs = [ (foldl1 (^+^) (map fst l), snd . head $ l) | l<-xs ]

        filterSumsT :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [(a,b)] -> [(a,b)]
        filterSumsT xs = [ x | x<-xs, and [fst x /= zeroV, snd x /= zeroV]]

        showSumT :: (Ord a, AdditiveGroup a, Ord b, AdditiveGroup b) => [(a,b)] -> [(a,b)]
        showSumT = filterSumsT . combineSumsT2 . gatherSumsT2 . combineSumsT1 . gatherSumsT1

        zModule :: (Int,Chain) -> Chain
        zModule (n, x) = n<**>x

        crossMap :: (a->b->c)->[a]->[b]->[c]
        crossMap f [] _ = []
        crossMap f _ [] = []
        crossMap f xs [y] = [ f x y | x<-xs ]
        crossMap f [x] ys = [ f x y | y<-ys ]
        crossMap f (x:xs) ys = crossMap f [x] ys ++ crossMap f xs ys
    
        -- Breaks a path into pairs of lists of arrows
        -- Needed for the path decomposition function
        splitPath :: (Path a) => a ->[(Chain,Chain)]
        splitPath p     | pathArrows(path p) == [] = [(path p, path p)]
                        | otherwise = map (mapPair longComp longComp) (successiveMap splitAt (length (pathArrows (path p))) (pathArrows (path p)))

        -- Apply a function "f", which depends on a number "m" and a parameter "a", "m" times, recording the outputs in a list
        -- Can be easily changed to differentiate between the parameter "m" and the number of runs of the function
        -- by changing the signature to (Int->a->b)->(Int->Int)->Int->a->Int->[b]
        successiveMap :: (Int->a->b)->Int->a->[b]
        successiveMap f (-1) x = []
        successiveMap f m x = [f n x | n <- [0..m]]

        -- Same as above, but now taking a function whose input is a list and concatenating the outputs instead of making a
        -- list of lists
        successiveListMap :: (Int->a->[b])->Int->a->[b]
        successiveListMap f (-1) x = []
        successiveListMap f m x = (successiveListMap f (m-1) x) ++ f m x
        
        -- Gets all the possible words of arrows in the quiver with a given number of letters
        getWords :: Int -> Quiver -> [[Arrow]]
        getWords _ (Quiver n vs []) = []
        getWords n q = mapM (const $ arrows q) [1..n]

        -- Generalizes composition of two arrows to a finite list of arrows
        longComp :: (Path a) => [a]->Chain
        longComp [] = emptyPath
        longComp [x] = path x
        longComp (x:xs) = x#longComp xs

        -- Tries to compose all words of arrows
        getPaths :: [[Arrow]]->[Chain]
        getPaths [] = []
        getPaths (xs) = [ longComp x | x <- xs ]

        -- Basically combines getWords and getPaths into a single function
        getPathsFromWords :: Int -> Quiver -> [Chain]
        getPathsFromWords n q = getPaths (getWords n q)

        getPairs :: (Path a, Path b) => [a] -> [b] -> [Chain]
        getPairs [] _ = []
        getPairs _ [] = []
        getPairs as bs = filterEmptyPaths [ a#b | a<-as, b<-bs]

        getAllPaths :: (Path a, Path b) => [a] -> [b] -> [Chain]
        getAllPaths [] _ = []
        getAllPaths _ [] = []
        getAllPaths a b      | all (== emptyPath) (getPairs a b) = map path a
                                | otherwise = (map path a) ++ getAllPaths (getPairs a b) b

        -- Basic check for empty
        isEmptyPath :: Chain->Bool
        isEmptyPath = (emptyPath ==)

        -- The list returned by getAllPathsUnfiltired will, most like than not, be filled with "emptyPaths"
        -- This removes them, leaving only a list with proper paths
        filterEmptyPaths :: (Path a) => [a] -> [a]
        filterEmptyPaths (xs) = [ x | x <- xs, path x /= emptyPath]

        -- Removes the "Empty paths" when applying the (%) operator
        filterDeltaEmptyPaths :: (Chain,Chain) -> (Chain,Chain)
        filterDeltaEmptyPaths (x,y)     | isEmptyPath x = (path(source y),y)
                                        | isEmptyPath y = (x,path(target x))
                                        | otherwise = (x,y)

        arrowsFrom :: (Path a) => a -> Quiver -> [Arrow]
        arrowsFrom x q = [ a | a <- arrows q, source a == target x]

        unseenArrowsFrom :: (Path a) => a -> Quiver -> [Arrow]
        unseenArrowsFrom x q = [ b | b <- (arrowsFrom (path x) q), b `notElem` (pathArrows (path x))]

        pathsFrom :: (Path a) => a -> Quiver -> [Chain]
        pathsFrom x q   | unseenArrowsFrom x q == [] = []
                        | otherwise = map (x#) (unseenArrowsFrom x q) ++ concat [ pathsFrom p q | p<-(map (x#) (unseenArrowsFrom x q))]

        arrowsTo :: (Path a) => a -> Quiver -> [Arrow]
        arrowsTo x q = [ a | a <- arrows q, source x == target a]

        unseenArrowsTo :: (Path a) => a -> Quiver -> [Arrow]
        unseenArrowsTo x q = [ b | b <- (arrowsTo (path x) q), b `notElem` pathArrows (path x) ]

        pathsTo :: (Path a) => a -> Quiver -> [Chain]
        pathsTo x q   | unseenArrowsTo x q == [] = []
                            | otherwise = map (#x) (unseenArrowsTo x q) ++ concat [ pathsTo p q | p<-(map (#x) (unseenArrowsTo x q))]

        tailsFrom :: (Path a) => [a] -> Quiver -> [Chain]
        tailsFrom [] q = []
        tailsFrom xs q = (tailLoopsFrom xs q) ++ [ p | p<-paths q, not (xs `isTail` [p]), xs `isHead` [p], p `notElem` tailLoopsFrom xs q]

        headsTo :: (Path a) => [a] -> Quiver -> [Chain]
        headsTo [] q = []
        headsTo xs q = (headLoopsTo xs q) ++ [ p | p<-paths q, not (xs `isHead` [p]), xs `isTail` [p], p `notElem` headLoopsTo xs q]

        cellsFrom :: (Path a) => [a] -> Quiver -> [Chain]
        cellsFrom [] q = []
        cellsFrom xs q = [ p | p<-paths q, xs `isHead` [p], xs `isTail` [p] , (length . pathArrows) p >= 1 ]

        tailLoopsFrom :: (Path a) => [a] -> Quiver -> [Chain]
        tailLoopsFrom [] q = []
        tailLoopsFrom xs q = [ p | p<-paths q, xs `isHead` [p], length (pathArrows p) == 1]
        
        headLoopsTo :: (Path a) => [a] -> Quiver -> [Chain]
        headLoopsTo [] q = []
        headLoopsTo xs q = [ p | p<-paths q, xs `isTail` [p], length (pathArrows p) == 1]

        isHead :: (Path a, Path b) => [a] -> [b] -> Bool
        isHead [x]      ys = path x `elem` (pathHeads (map path ys))
        isHead (x:xs)   ys = or [ [x] `isHead` ys, xs `isHead` ys ]

        isTail :: (Path a, Path b) => [a] -> [b] -> Bool
        isTail [x]      ys = path x `elem` (pathTails (map path ys))
        isTail (x:xs)   ys = or [ [x] `isTail` ys, xs `isTail` ys ]

        pathHeads :: (Path a) => [a] -> [Chain]
        pathHeads = map fst . (snd =<<) . ((%) =<<)

        pathTails :: (Path a) => [a] -> [Chain]
        pathTails = map snd . (snd =<<) . ((%) =<<)

        -- Used in Show Arrow
        strJoin sep arr = case arr of
            [] -> ""
            [x] -> x
            (x : xs) -> x ++ sep ++ strJoin sep xs
        -- Used in Show Chain
        pathJoin arr = case arr of
            [] -> ""
            [x] -> vertexName (arrowSource x) ++ " -" ++ arrowName x ++ "-> " ++ vertexName (arrowTarget x)
            (x : xs) -> vertexName (arrowSource x) ++ " -" ++ arrowName x ++ "-> " ++ pathJoin xs        

        -- The empty path will be useful
        emptyPath :: Chain
        emptyPath = Chain "" [] (Vertex "") (Vertex "")

        -- The "do nothing" path at a vertex that corresponds to "staying at home at the vertex"
        stationaryPath :: Vertex->Chain
        stationaryPath v = Chain ("e" ++ vertexName v) [] (Vertex (vertexName v)) (Vertex (vertexName v))

        -- The basic paths corresponding to each arrow
        arrowPath :: Arrow -> Chain
        arrowPath a = Chain (arrowName a) [a] (Vertex (vertexName (source a))) (Vertex (vertexName (target a)))

        toVertex :: (Path a) => a -> Vertex
        toVertex x  | (length . pathArrows . path) x == 0 = Vertex (stripChars "e" ((pathName . path) x))
                    | otherwise = Vertex ((pathName . path) x)

        toArrow :: (Path a) => a -> Arrow
        toArrow x = Arrow ((pathName . path) x) ((source . path) x) ((target . path) x)

        stripChars :: String -> String -> String
        stripChars = filter . flip notElem

    -- End Auxiliary