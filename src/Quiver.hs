module Quiver (
    Quiver (..)
    , Vertex(..)
    , Arrow(..)
    , Chain(..)
    , Path(..)
    , (#)
    , (%)
    , emptyPath
    , maxPathLength
    , hasCycles
    , longComp
    , stationaryPath
    , arrowPath
    , paths
    , newPathsFrom
    -- , pathsTo
    , arrowsFrom
    , adjacentVertices
    , seenArrows
    ) where
        import Data.List
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

        -- Actual definition of how to compose two composable types
        (#) :: (Path a, Path b) => a->b->Chain
        (#) a b = path (a,b)

        -- The inverse operation to composing
        -- Breaks a path into all its possible subpaths
        (%) :: Chain -> [(Chain,Chain)]
        (%) (Chain n [] s t) = [((Chain n [] s t),(Chain n [] s t))]
        (%) p = (map filterDeltaEmptyPaths . map (mapPair longComp longComp) . splitPath) p

        -- Gets the maximum size of a path in a quiver
        maxPathLength :: Quiver -> Int
        maxPathLength (Quiver n vs []) = 0
        maxPathLength (Quiver _ _ as) = length as

        
        paths :: Quiver -> [Chain]
        paths q = [ stationaryPath v | v <- vertices q] ++ sort (concatMap (flip pathsFrom q) (vertices q))

        -- Checks if a quiver has cycles
        -- This works because in a set with "n" symbols, the maximal word with distinc symbols has precisely all the symbols
        -- So if a path has more than maxPathLegth arrows, we know it's a cycle
        hasCycles :: Quiver -> Bool
        hasCycles (Quiver n vs []) = False
        hasCycles q = filterEmptyPaths (getPathsFromWords (succ (maxPathLength q)) q) /= []

    -- End Exported

    -- Begin Auxiliary

        -- "Path" class of things that are path-like
        class Path a where
            source :: a -> Vertex
            target :: a -> Vertex
            path :: a -> Chain
            pathLength :: a -> Int

        -- Redefining the show for each type
        -- Can be easily rewritten to show different things
        instance Show Vertex where
            show (Vertex s) = s

        -- Show arrow is done by literally drawing the arrow between its source and target vertices
        -- so we need some string manipulation
        instance Show Arrow where
            {- uncomment this to allow verbose shows - i.e. showing 1 -a-> 2 instead of just a 
            show (Arrow a s t) = show s ++ " -" ++ a ++ "-> " ++ show t -} 
            show = arrowName

        instance Show Chain where
            {- uncomment this to allow verbose shows - i.e. showing 1 -a-> 2 -b-> 3 instead of just ab
            show (Chain "" []) = "Empty path"
            show (Chain n as) = n ++ ": " ++ pathJoin as -}
            show (Chain n as s t) = n

        -- Sadly we cannot draw the quivers as they will, more often than not, be non-planar
        instance Show Quiver where
            show (Quiver s v a) = "Quiver " ++ s ++ ": {Vertices: " ++ (strJoin ", " (map show v)) ++ ". Arrows: " ++ (strJoin ", " (map show a)) ++ "}"

        -- Redefining equality for our types
        instance Eq Vertex where
            (Vertex s) == (Vertex t) = s == t

        instance Eq Arrow where
            (Arrow n s t) == (Arrow m d y) = n == m && s == d && t == y

        instance Eq Chain where
            (Chain n as s t) == (Chain m bs s' t')      | and [as == [], bs == []] = n == m
                                                        | otherwise = as == bs

        instance Eq Quiver where
            (Quiver n vs as) == (Quiver m ws bs) = n == m && vs == ws && as == bs

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

        -- Now we define pairs of path types as also being paths
        instance (Path a, Path b) => Path (a,b) where
            source (a,b) = source a
            target (a,b) = target b
            path (a,b) = (path a)<+>(path b)
            pathLength (a,b) = pathLength a + pathLength b

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

        -- Maps a pair of maps "f :: a->c" and "g :: b->d" to a pair "(a,b)" to obtain a pair "(c,d)"
        mapPair :: (a-> c)->(b-> d)->(a,b)->(c, d)
        mapPair f g (x,y) = (f x, g y)
    
        -- Breaks a path into pairs of lists of arrows
        -- Needed for the path decomposition function
        splitPath :: Chain->[([Arrow],[Arrow])]
        splitPath p = successiveMap splitAt (length (pathArrows p)) (pathArrows p)

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

        -- pathsFrom :: (Path a) => a-> Quiver -> [Chain]
        -- pathsFrom x q = filterEmptyPaths(map ((path x)#) (paths q))

        -- pathsTo :: (Path a) => a -> Quiver -> [Chain]
        -- pathsTo x q = filterEmptyPaths(map (#(path x)) (paths q))

        -- Basic check for empty
        isEmptyPath :: Chain->Bool
        isEmptyPath = (emptyPath ==)

        -- The list returned by getAllPathsUnfiltired will, most like than not, be filled with "emptyPaths"
        -- This removes them, leaving only a list with proper paths
        filterEmptyPaths :: (Path a) => [a] -> [a]
        filterEmptyPaths (xs) = [ x | x <- xs, (path x) /= emptyPath]

        -- Removes the "Empty paths" when applying the (%) operator
        filterDeltaEmptyPaths :: (Chain,Chain) -> (Chain,Chain)
        filterDeltaEmptyPaths (x,y)     | x == emptyPath = (path(source y),y)
                                        | y == emptyPath = (x,path(target x))
                                        | otherwise = (x,y)

        arrowsFrom :: (Path a) => a -> Quiver -> [Arrow]
        arrowsFrom x q = [ a | a <- arrows q, source a == target x]

        newPathsFrom :: (Path a) => a -> Quiver -> [Arrow] -> [Chain]
        newPathsFrom x q as | newArrowsFrom x q as == [] = []
                            | otherwise = map (x#) (newArrowsFrom x q as) ++ concatMap (reverseNewPathsFrom q (seenArrows x q as)) (map (x#) (newArrowsFrom x q as))

        reverseNewPathsFrom :: (Path a) => Quiver -> [Arrow] -> a -> [Chain]
        reverseNewPathsFrom q as x = newPathsFrom x q as

        newArrowsFrom :: (Path a) => a -> Quiver -> [Arrow] -> [Arrow]
        newArrowsFrom x q as = [ b | b <- (arrowsFrom (path x) q), b `notElem` as ]

        arrowsTo :: (Path a) => a -> Quiver -> [Arrow]
        arrowsTo x q = [ a | a <- arrows q, target a == source x]

        adjacentVertices :: (Path a) => a -> Quiver -> [Vertex]
        adjacentVertices x q = [ v | v <- map target (arrowsFrom x q)] ++ [ v | v <- map source (arrowsTo x q)]

        seenArrows :: (Path a) => a -> Quiver -> [Arrow] -> [Arrow]
        seenArrows x q as = as ++ newArrowsFrom x q as

        pathsFrom :: (Path a) => a -> Quiver -> [Chain]
        pathsFrom x q = newPathsFrom x q (pathArrows(path x))


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

    -- End Auxiliary