import qualified Data.Heap as H
import qualified Data.List as L

data SymbolTree = Leaf { symb  :: Char 
                       , freq  :: Int
                       }
                | Tree { freq  :: Int
                       , left  :: SymbolTree
                       , right :: SymbolTree
                       } deriving (Show, Eq)

instance Ord SymbolTree where
    a `compare` b = freq a `compare` freq b

type Queue = H.MinHeap SymbolTree
type Code  = [(Char, String)]

huffman :: [(Char, Int)] -> Code
huffman hs = 
    let symbols     = map (uncurry Leaf) hs
        symbolQueue = H.fromList symbols :: Queue
        symbolTree  = genTree symbolQueue
    in  L.sort $ encode symbolTree

genTree :: Queue -> SymbolTree
genTree q
    | H.size q == 1 = let Just t = H.viewHead q in t
    | otherwise = 
        let ([a, b], rest) = H.splitAt 2 q
            t = nextTree a b
        in genTree $ H.insert t rest

nextTree :: SymbolTree -> SymbolTree -> SymbolTree
nextTree a b = Tree (freq a + freq b) a b

encode :: SymbolTree -> Code
encode t@(Leaf _ s) = parseTree t "0"
encode (Tree _ left right) = 
    let fixString (c, s) = (c, reverse s)
    in  map fixString $ parseTree left "0" ++ parseTree right "1"

parseTree :: SymbolTree -> String -> Code
parseTree (Leaf s _) prefix = [(s, prefix)]
parseTree (Tree _ left right) prefix = 
    let small = parseTree left  ('0':prefix) 
        large = parseTree right ('1':prefix)
    in small ++ large

exercise :: [(Char, Int)]
exercise = [('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5)]
