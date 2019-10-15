import Data.List
import Data.Ord

any' :: (a -> Bool) -> [a] -> Bool
any' p (x:xs) = p x || any' p xs
any' p []     = False

-- ZF expressions / liste en comprehension

rectangle :: [(Int,Int)]
rectangle = [ (x,y) | x<-[1..n], y <-[1..m] ]
  where n = 3
        m = 2

triangle :: [(Int,Int)]
triangle = [ (x,y) | x<-[1..n], y <-[1..m], y<=x ]
  where n = 4
        m = 5

triangle' :: [(Int,Int)]
triangle' = [ (x,y) | x<-[1..n], y <-[1..x] ]
  where n = 4
        m = 5

myQSort :: Ord a => [a] -> [a]
myQSort (x:xs) = myQSort [y | y <- xs, y<= x] ++ x : myQSort [ y | y <- xs, y>x ]
myQSort []     = []

-- but du TP : trouver les solutions pour le compte est bon

-- sous listes et permutations pour considerer les nombres utilises dans le calcul

-- sous liste

-- deja vu au bloc 2
sousListes :: [a] -> [[a]]
sousListes []     = [[]]
sousListes (x:xs) = ys ++ map (x:) ys
    where ys = sousListes xs

injections :: a -> [a] -> [[a]]
injections y (x:xs) = (y:x:xs) : map (x:) (injections y xs)
injections y []     = [[y]]

permuts :: [a] -> [[a]]
permuts (x:xs) = concat (map (injections x) (permuts xs))
permuts []     = [[]]

permSousListes :: [a] -> [[a]]
permSousListes xs = [zs | ys <- sousListes xs, not (null ys), zs <- permuts ys]

partitionStricte :: [a] -> [([a],[a])]
partitionStricte [x1,x2] = [([x1],[x2])]
partitionStricte (x1:xs) = ([x1],xs) : map (\(ls,rs) -> (x1:ls,rs)) (partitionStricte xs)

-- I) generate and test (brute force)

data Op = Add | Sub | Mul | Div deriving Eq --deriving Show

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

validOp :: Op -> Int -> Int -> Bool
validOp Sub x y = x>y
validOp Div x y = y/=0 && x `mod` y==0
validOp _   _ _ = True

evalOp :: Op -> Int -> Int -> Int
evalOp Add x y = x+y
evalOp Sub x y = x-y
evalOp Mul x y = x*y
evalOp Div x y = x `div` y

data Exp = Val Int | App Op Exp Exp
            deriving Show

-- step1: enumerate expressions
exps :: [Int] -> [Exp]
exps [n] = [Val n]
exps ns = [ App o g d
  | (gs,ds) <- partitionStricte ns
  , g <- exps gs
  , d <- exps ds
  , o <- [Add,Sub,Mul,Div]
  ]

-- step2: filter out invalid expressions

evalExp :: Exp -> Int
evalExp (App o l r) = evalOp o (evalExp l) (evalExp r)
evalExp (Val n) = n

validExp :: Exp -> Bool
validExp (Val n) = n>0
validExp (App o l r) = validExp l && validExp r && validOp o (evalExp l) (evalExp r)

solutions :: [Int] -> Int -> [Exp]
solutions nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps ns)
      es' = filter validExp es
      es'' = filter (\e -> evalExp e ==cible) es'
  in es''

test1 = solutions [1,3,7,10,25,50] 765


-- II) fusionner la generation et le filtrage des expressions invalides

exps2 :: [Int] -> [Exp]
exps2 [n] = [Val n]
exps2 ns = [ App o g d
  | (gs,ds) <- partitionStricte ns
  , g <- exps2 gs
  , d <- exps2 ds
  , o <- [Add,Sub,Mul,Div]
  , validExp (App o g d)
  ]


solutions2 :: [Int] -> Int -> [Exp]
solutions2 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps2 ns)
      es' = filter (\e -> evalExp e == cible) es
  in es'

test2 = solutions2 [1,3,7,10,25,50] 765


-- III) memoiser l'evaluation

data Exp' = Val' Int | App' Op Exp' Exp' Int
            deriving Show

evalExp' :: Exp' -> Int
evalExp' (Val' n) = n
evalExp' (App' _ _ _ n) = n

exps3 :: [Int] -> [Exp']
exps3 [n] = [Val' n]
exps3 ns = [App' o g d (evalOp o (evalExp' g) (evalExp' d))
  | (gs,ds) <- partitionStricte ns
  , o <- [Add, Sub, Mul, Div]
  , g <- exps3 gs
  , d <- exps3 ds
  , validOp o (evalExp' g) (evalExp' d)
  ]

solutions3 :: [Int] -> Int -> [Exp']
solutions3 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps3 ns)
      es' = filter (\e -> evalExp' e == cible) es
  in es'

test3 = solutions3 [1,3,7,10,25,50] 765


-- IV) exploiter des proprietes arithmetiques

-- pour reduire l'espace de recherche on ajoute les regles :
-- - pas de multiplication par 1
-- - pas de division par 1
-- - addition et multiplication commutatives (ne considerer qu'un sens (quand les deux operandes sont differents))
validOp' :: Op -> Int -> Int -> Bool
validOp' Sub x y = x>y
validOp' Div x y = y/=1 && y/=0 && x `mod` y==0
validOp' Mul x y = y/=1 && x>=y
validOp' Add x y = x>=y

exps4 :: [Int] -> [Exp']
exps4 [n] = [Val' n]
exps4 ns = [App' o g d (evalOp o (evalExp' g) (evalExp' d))
  | (gs,ds) <- partitionStricte ns
  , o <- [Add, Sub, Mul, Div]
  , g <- exps4 gs
  , d <- exps4 ds
  , validOp' o (evalExp' g) (evalExp' d)
  ]

solutions4 :: [Int] -> Int -> [Exp']
solutions4 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps4 ns)
      es' = filter (\e -> evalExp' e == cible) es
  in es'

test4 = solutions4 [1,3,7,10,25,50] 765

-- nombre de solutions

nombreDeSolutions3 = length test3
nombreDeSolutions4 = length test4

-- V) ne retourner qu'une solution exacte ou bien la plus proche

solutions5 :: [Int] -> Int -> [Exp']
solutions5 nombres cible =
  let ns = permSousListes nombres
      es = concat (map exps4 ns)
      es' = filter (\e -> evalExp' e == cible) es
  in es'

test5 = solutions5 [1,3,7,10,25,50] 765
test6 = solutions5 [1,3,7,10,25,50] 831

-- VI) affichez les expressions sous forme infixe en evitant des parentheses inutiles
--instance Show Exp' where
--    show :: Exp' -> String
--    show e = undefined

-- VII) generalisez certaines fonctions avec de l'ordre superieur afin de reduire la duplication de code dans ce programme

-- misc : cherchez les solutions avec le moins d'operations en priorite