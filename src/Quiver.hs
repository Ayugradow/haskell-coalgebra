{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Quiver (
    Quiver (..),
    Vertex(..),
    Arrow(..),
    Path(..),
    Comp(..),
    (<+>),
    (#),
    maxPathLength,
    longComp,
    getAllPaths,
    hasCycles,
    getWords,
    getPaths
    ) where
    data Vertex = Vertex {
        vertexName :: String
    }

    data Arrow = Arrow {
        arrowName :: String,
        arrowSource :: Vertex,
        arrowTarget :: Vertex
    }

    data Quiver = Quiver {
        quiverName :: String,
        vertices :: [Vertex],
        arrows :: [Arrow]
    }

    data Path = Path {
        pathName :: String,
        pathArrows :: [Arrow],
        pathSource :: Vertex,
        pathTarget :: Vertex
    }

    class Comp a where
        source :: a -> Vertex
        target :: a -> Vertex
        toPath :: a -> Path

    emptyPath :: Path
    emptyPath = Path {pathName = "", pathArrows = [], pathSource = Vertex {vertexName = ""}, pathTarget = Vertex {vertexName = ""}}

    stationaryPath :: Vertex->Path
    stationaryPath v = Path {pathName = "", pathArrows = [], pathSource = v, pathTarget = v}

    arrowPath :: Arrow -> Path
    arrowPath a = Path {pathName = arrowName a, pathArrows = [a], pathSource = arrowSource a, pathTarget = arrowTarget a}
    
    instance Show Vertex where
        show (Vertex s) = s

    instance Show Arrow where
        show (Arrow a s t) = show s ++ " -" ++ a ++ "-> " ++ show t

    strJoin sep arr = case arr of
        [] -> ""
        [x] -> x
        (x : xs) -> x ++ sep ++ strJoin sep xs

    pathJoin arr = case arr of
        [] -> ""
        [x] -> vertexName (arrowSource x) ++ " -" ++ arrowName x ++ "-> " ++ vertexName (arrowTarget x)
        (x : xs) -> vertexName (arrowSource x) ++ " -" ++ arrowName x ++ "-> " ++ pathJoin xs

    (<+>) :: Path->Path->Path
    x<+>y
        | pathTarget x == pathSource y = Path {pathName = (pathName x ++ pathName y), pathArrows = (pathArrows x ++ pathArrows y), pathSource = pathSource x, pathTarget = pathTarget y}
        | otherwise = emptyPath

    instance Show Path where
        show (Path "" [] Vertex {vertexName = ""} Vertex {vertexName = ""}) = "Empty path"
        show (Path n as v w) = "Path " ++ n ++ ": " ++ pathJoin as

    instance Show Quiver where
        show (Quiver s v a) = "Quiver " ++ s ++ ": {Vertices: " ++ (strJoin ", " (map show v)) ++ ". Arrows: " ++ (strJoin ", " (map show a)) ++ "}"

    instance Eq Vertex where
        (Vertex s) == (Vertex t) = s == t

    instance Eq Arrow where
        (Arrow n s t) == (Arrow m d y) = n == m && s == d && t == y

    instance Eq Path where
        (Path n as s t) == (Path m bs r u) = as == bs

    instance Eq Quiver where
        (Quiver n vs as) == (Quiver m ws bs) = n == m && vs == ws && as == bs
    
    instance Comp Path where
        source = pathSource
        target = pathTarget
        toPath = id        
    
    instance Comp Vertex where
        source = id
        target = id
        toPath = stationaryPath

    instance Comp Arrow where
        source = arrowSource
        target = arrowTarget
        toPath = arrowPath

    instance (Comp a, Comp b) => Comp (a,b) where
        source (a,b) = source a
        target (a,b) = target b
        toPath (a,b) = (toPath a)<+>(toPath b)

    (#) :: (Comp a, Comp b) => a->b->Path
    (#) a b = toPath (a,b)    
    
    getWords :: Quiver -> Int -> [[Arrow]]
    getWords (Quiver n vs []) _ = []
    getWords q n  = mapM (const $ arrows q) [1..n]

    longComp :: [Arrow]->Path
    longComp = foldl (#) =<< stationaryPath . source . head

    getPaths :: [[Arrow]]->[Path]
    getPaths [] = []
    getPaths (x:xs) = [longComp x] ++ getPaths xs

    allTheSame :: (Eq a) => [a] -> Bool
    allTheSame xs = and $ map (== head xs) (tail xs)

    while :: Int -> (Int -> Bool) -> (Int -> [Path]) -> [Path]
    while start condition f
        | condition start = f start
        | otherwise = f start ++ while (start + 1) condition f

    -- getMaxPathLength :: [Arrow] -> Int
    -- getMaxPathLength [] = 0
    -- getMaxPathLength as = ((while 1 (allTheSame.getPaths.(getWords as)) (getPaths.(getWords as)))-1)

    maxPathLength :: Quiver -> Int
    maxPathLength (Quiver n vs []) = 0
    maxPathLength (Quiver _ _ as) = length as

    isEmptyPath :: Path->Bool
    isEmptyPath = (emptyPath ==)

    getAllPathsUnfiltered :: Quiver -> [Path]
    getAllPathsUnfiltered (Quiver n vs []) = []
    getAllPathsUnfiltered q =  while 1 (\x -> x == maxPathLength q) (getPaths.(getWords q))

    filterEmptyPaths :: [Path] -> [Path]
    filterEmptyPaths = filter (/= emptyPath)

    getAllPaths :: Quiver -> [Path]
    getAllPaths = filterEmptyPaths.getAllPathsUnfiltered

    hasCycles :: Quiver -> Bool
    hasCycles (Quiver n vs []) = False
    hasCycles q = filterEmptyPaths(getPaths(getWords q ((maxPathLength q)+1))) /= []