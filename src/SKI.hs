module SKI where

import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Data.Char(isLetter)

i :: a -> a
i = id

k :: a -> b -> a
k = const

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = (x z) (y z)

data E = Leaf T | Node E E deriving(Show, Eq)
data T = I | K | S deriving(Show, Eq)

reduce :: E -> E
reduce expr = case expr of
    Leaf t -> Leaf t
    Node (Node (Leaf K) (Leaf l)) n2 -> Leaf l
    Node (Node (Leaf S) (Leaf l)) (Node n1 n2) -> Node (Node (Leaf l) n2) (Node n1 n2)
    Node e1 e2 -> case e1 of
        Leaf I -> e2
        Leaf K -> case e2 of
            Node ex _ -> ex
            Leaf t -> Node (Leaf K) (Leaf t)
        Leaf S -> case e2 of
            Node (Leaf t1) (Node (Leaf t2) (Leaf t3)) ->
                                Node (Node (Leaf t1) (Leaf t3)) (Node (Leaf t2) (Leaf t3))
            Node (Node (Leaf t1) (Leaf t2)) (Leaf t3) ->
                                Node (Node (Leaf t1) (Leaf t3)) (Node (Leaf t2) (Leaf t3))
            n -> Node (Leaf S) (reduce n)
        Node n1 n2 -> Node (reduce n1) (Node n2 e2)

parse :: String -> Maybe E
parse s = case (readP_to_S expr s) of
    xs -> Just $ fst . last $ xs
    _       -> Nothing

leaf :: ReadP E
leaf = do
    sym <- satisfy (\c -> c `elem` ['S', 'K', 'I'])
    return $ Leaf $ readSym sym

readSym :: Char -> T
readSym c = case c of
    'S' -> S
    'K' -> K
    'I' -> I
    _ -> undefined

expr :: ReadP E
expr = node  -- +++ parens

-- parens :: ReadP E
-- parens = do
--    between (char '(') (char ')') expr


node :: ReadP E
node = do
    leaflist <- many1 leaf
    return $ tree leaflist

tree :: [E] -> E
tree [] = undefined
tree [x] = x
tree (x:xs) = Node x (tree xs)

read :: IO ()
read = do
  args <- getArgs
  putStrLn $ concat args
