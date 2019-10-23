
-- questions ?

-- decidable
-- complexite (dimensions)
-- test
-- debugger




import Test.QuickCheck
import Data.Char

import Control.Monad
import Control.Monad.Trans.State

myAppend :: [a] -> [a] -> [a]
myAppend (x:xs) ys = x:myAppend xs ys
myAppend []     ys = ys
  
myAppendProp1 :: ([Int],[Int]) -> Bool
myAppendProp1 (xs,ys) = length (myAppend xs ys) == length xs + length ys

myNotProp1 :: Bool -> Bool
myNotProp1 b = not (not b) == b

testB44 = verboseCheck myNotProp1

test = quickCheck myAppendProp1
testV = verboseCheck myAppendProp1

-- random

-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_aléatoires
-- http://fr.wikipedia.org/wiki/Générateur_de_nombres_pseudo-aléatoires
-- Générateurs congruentiels linéaires

-- a sequence of integers with a long period and a fair distribution

u :: Int -> Int
u n = (16807 * n) `mod` (2^31-1)

-- a global constant (a LAZY infinite list) 

type Random = [Int]

random :: Random
random = drop 2 (iterate u 1)

test1 = take 10 random

-- generate a random value

type Generator a = Random -> (a,Random)

rBool :: Generator Bool
--rBool :: Random -> (Bool,Random)
rBool (i:is) = (even i,is)

r3Bools :: Generator (Bool,Bool,Bool)
r3Bools random = 
    let (b1,random1) = rBool random
        (b2,random2) = rBool random1
        (b3,random3) = rBool random2
    in ((b1,b2,b3),random3)

test2 = fst (r3Bools random)

-- generate a list of bools

rListBool :: Generator [Bool]
rListBool random = 
    let (isNil,random1) = rBool random 
    in if isNil 
       then ([],random1)
       else let (head,random2) = rBool     random1
                (tail,random3) = rListBool random2
            in (head:tail,random3)

test3 = fst (rListBool random)

-- more cons than nil

rPercent :: Generator Int
rPercent (i:is) = (i `mod` 100,is)

rListBool' :: Generator [Bool]
rListBool' random = 
    let (isNil,random1) = rPercent random 
    in if (isNil<20) 
       then ([],random1)
       else let (head,random2) = rBool      random1
                (tail,random3) = rListBool' random2
            in (head:tail,random3)

test3' = fst (rListBool' random)

-- generate other type of elements

rChar :: Generator Char
rChar (i:is) = (chr (ord 'a' + i `mod` 26),is)

rList :: Generator a -> Generator [a]
rList genElt random = 
    let (isNil,random1) = rPercent random 
    in if (isNil<20) 
       then ([],random1)
       else let (head,random2) = genElt       random1
                (tail,random3) = rList genElt random2
            in (head:tail,random3)

test4 = fst (rList rChar random)

test3'' = fst (rList rBool random)


-- a quickcheck like

class Generatable a where
    myGenerate :: Generator a -- rappel : Random -> (a,Random)

instance Generatable Int where
    myGenerate (r:rs) = (r,rs)

instance Generatable Bool where
    myGenerate (r:rs) = (even r,rs)

instance Generatable Char where
    myGenerate (r:rs) = (chr (ord 'a' + r `mod` 26),rs)

instance Generatable a => Generatable [a] where
    myGenerate random = 
        let (isNil,random1) = rPercent random 
        in if (isNil<30)
           then ([],random1)
           else let (head,random2) = myGenerate random1 -- myGenerate is called here
                    (tail,random3) = myGenerate random2
                in (head:tail,random3)

-- yet another type
instance (Generatable a, Generatable b) => Generatable (a,b) where
    myGenerate random = 
        let (a,random1) = myGenerate random
            (b,random2) = myGenerate random1
        in ((a,b),random2)

-- type base selection

testB :: [(Bool,[[String]])]
testB = fst (myGenerate random)


--testB :: [Int]
--testB :: [Bool]
--testB :: [[Bool]]
--testB :: [(Int,[Bool])]
--testB :: [(Int,[(Bool,String)])]

appendProp1' :: ([Int],[Int]) -> Bool
appendProp1' (xs,ys) = length (xs++ys) == length xs + length ys 

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr ((&&) . p) True

myQuickCheck :: Generatable a => (a -> Bool) -> Bool
myQuickCheck prop = myAll prop (fst (myGenerate random))

myTest = myQuickCheck appendProp1'


generateInfinity :: Generatable a => Generator [a]
generateInfinity random =
  let (head,random1) = myGenerate random 
      (tail,random2) = generateInfinity random1
  in (head:tail,random2)


myQuickCheck' :: Generatable a => (a -> Bool) -> Bool
--myQuickCheck' prop = myAll prop (fst (generateInfinity random))
myQuickCheck' prop = myAll prop (take 100 (fst (generateInfinity random)))


-- welcome to the monadic world

type GeneratorM a = State Random a 

class GeneratableM a where
    myGenerateM :: GeneratorM a 

instance GeneratableM Int where
    --myGenerate (r:rs) = (r,rs)
  myGenerateM = do
    (r:rs) <- get
    put rs
    return r

isInt :: Int -> Int
isInt x = x

instance GeneratableM Bool where
    -- myGenerate (r:rs) = (even r,rs)
  myGenerateM = do
    i <- myGenerateM
    return (even (isInt i))

instance GeneratableM Char where
    --myGenerateM (r:rs) = (chr (ord 'a' + r `mod` 26),rs)
  myGenerateM = do
    i <- myGenerateM
    return (chr (ord 'a' + i `mod` 26))

instance GeneratableM a => GeneratableM [a] where
{-
    myGenerateM random = 
        let (isNil,random1) = rPercent random 
        in if (isNil<30)
           then ([],random1)
           else let (head,random2) = myGenerateM random1 -- myGenerateM is called here
                    (tail,random3) = myGenerateM random2
                in (head:tail,random3)
-}
  myGenerateM = do
    i <- myGenerateM
    if ((isInt i) `mod` 100 < 30)
      then return []
      else do
      b <- myGenerateM
      bs <- myGenerateM
      return (b:bs)

instance (GeneratableM a, GeneratableM b) => GeneratableM (a,b) where
{-
    myGenerate random = 
        let (a,random1) = myGenerate random
            (b,random2) = myGenerate random1
        in ((a,b),random2)
-}
  myGenerateM = do
    a <- myGenerateM
    b <- myGenerateM
    return (a,b)

test42 :: Bool
test42 = myAll myAppendProp1 (evalState myGenerateM random)


-- when the state is not random::[Int] but real world

main :: IO ()
main = do
 putStrLn "quel est ton nom ?"
 r <- getLine
 putStrLn ("bonjour " ++ r)
 
