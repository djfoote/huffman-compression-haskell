import qualified Data.Map           as Map
import qualified Data.Heap          as Heap
import qualified Data.List          as List
import qualified System.Environment as Env
import qualified Control.Monad      as M

data SymbolTree = Leaf { symb  :: Char 
                       , freq  :: Int
                       }
                | Tree { freq  :: Int
                       , left  :: SymbolTree
                       , right :: SymbolTree
                       } deriving (Show, Eq)

instance Ord SymbolTree where
    a `compare` b = freq a `compare` freq b

type Queue = Heap.MinHeap SymbolTree
type Encoding = [(Char, String)]

huffman :: [(Char, Int)] -> Encoding
huffman hs = 
    let symbols     = map (uncurry Leaf) hs
        symbolQueue = Heap.fromList symbols :: Queue
        symbolTree  = genTree symbolQueue
    in  List.sort $ encode symbolTree

genTree :: Queue -> SymbolTree
genTree q
    | Heap.size q == 1 = let Just t = Heap.viewHead q in t
    | otherwise = 
        let ([a, b], rest) = Heap.splitAt 2 q
            t = nextTree a b
        in genTree $ Heap.insert t rest

nextTree :: SymbolTree -> SymbolTree -> SymbolTree
nextTree a b = Tree (freq a + freq b) a b

encode :: SymbolTree -> Encoding
encode t@(Leaf _ s) = parseTree t "0"
encode (Tree _ left right) = 
    let fixString (c, s) = (c, reverse s)
    in  map fixString $ parseTree left "0" ++ parseTree right "1"

parseTree :: SymbolTree -> String -> Encoding
parseTree (Leaf s _) prefix = [(s, prefix)]
parseTree (Tree _ left right) prefix = 
    let small = parseTree left  ('0':prefix) 
        large = parseTree right ('1':prefix)
    in  small ++ large

toBits :: String -> Encoding -> Maybe String
toBits s code = M.foldM (\prev c -> fmap (prev ++) $ List.lookup c code) "" s

getCounts :: String -> [(Char, Int)]
getCounts = Map.toList . Map.fromListWith (+) . map (\c -> (c, 1))

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (first, rest) = splitAt n xs in first : groupsOf n rest

compress :: String -> String
compress str = let Just s = toBits str . huffman . getCounts $ str in s

compressionFactor :: String -> Float
compressionFactor str = 
    fromIntegral (length $ compress str) / fromIntegral (8 * length str)

main = do 
    args <- Env.getArgs
    contents <- readFile $ head args
    putStrLn $ compress contents
