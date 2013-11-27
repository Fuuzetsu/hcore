module Core.Utils where

import Data.List (genericLength, sort)

-- We don't have Miranda's abstype so we use a regular
-- data type and provide some functions on it
type Number = Double

-- We cheat and just use integers in places where we would anyway
-- instead of trying to emulate Miranda's ‘num’ with Doubles.
type NumberI = Integer

-- | Miranda built-in
shownum :: (Show a, Num a) => a -> String
shownum = show

-- Heap, A.1

hInitial :: Heap a
hInitial = (0, [1 ..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a,n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size - 1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size, free, cts) a = aLookup cts a
                              (error ("can't find node " ++ showaddr a ++ " in a heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Number
hSize (size, free, cts) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

showaddr :: Addr -> [Char]
showaddr a = "#" ++ shownum a

remove :: [(Number, a)] -> Number -> [(Number, a)]
remove [] a = error ("Attempt to update or free nonexistent address #"
                     ++ shownum a)
remove ((a', n):cts) a
  | a == a' = cts
  | otherwise = (a', n) : remove cts a

type Heap a = (Number, [Number], [(Number, a)])
type Addr = Double

-- Association list, A.2

type Assoc a b = [(a, b)]

aLookup :: Eq a => Assoc a b -> a -> b -> b
aLookup [] k' def = def
aLookup ((k, v):bs) k' def
  | k == k' = v
  | otherwise = aLookup bs k' def

aDomain :: Assoc a b -> [a]
aDomain alist = [key | (key, val) <- alist]

aRange :: Assoc a b -> [b]
aRange alist = [val | (key, val) <- alist]

aEmpty :: Assoc a b
aEmpty = []

-- Generating unique names, A.3

getName :: NameSupply -> [Char] -> (NameSupply, [Char])
getName nameSupply prefix = (nameSupply + 1, makeName prefix nameSupply)

getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
getNames nameSupply prefixes =
  (nameSupply + l prefixes, zipWith makeName prefixes [nameSupply ..])
  where
    l = genericLength

initialNameSupply :: NameSupply
initialNameSupply = 0

makeName prefix ns = prefix ++ "_" ++ shownum ns

-- We use Integer instead of Double in this case because
-- that's what the book seems to rely on Miranda using. We don't
-- get the automatic Double ↔ Integer conversion but it should not
-- be necessary with this type.
type NameSupply = Integer

-- Sets, A.4

setFromList :: Ord a => [a] -> Set a
setFromList = rmdup . sort
  where
    rmdup []        = []
    rmdup [x]       = [x]
    rmdupt (x:y:xs)
      | x == y      = rmdup (y:xs)
      | otherwise   = x : rmdup (y:xs)

setToList :: Set a -> [a]
setToList xs = xs

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion [] []         = []
setUnion [] (b:bs)     = (b:bs)
setUnion (a:as) []     = (a:as)
setUnion (a:as) (b:bs)
  | a < b              = a : setUnion as (b:bs)
  | a == b             = a : setUnion as bs
  | a > b              = b : setUnion (a:as) bs

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection [] []         = []
setIntersection [] (b:bs)     = []
setIntersection (a:as) []     = []
setIntersection (a:as) (b:bs)
  | a < b                     = setIntersection as (b:bs)
  | a == b                    = a : setIntersection as bs
  | a > b                     = setIntersection (a:as) bs

setSubtraction :: Ord a => Set a -> Set a -> Set a
setSubtraction [] []         = []
setSubtraction [] (b:bs)     = []
setSubtraction (a:as) []     = (a:as)
setSubtraction (a:as) (b:bs)
  | a < b                    = a : setSubtraction as (b:bs)
  | a == b                   = setSubtraction as bs
  | a > b                    = setSubtraction (a:as) bs


setElementOf :: Ord a => a -> Set a -> Bool
setElementOf x []     = False
setElementOf x (y:ys) = x == y || (x > y && setElementOf x ys)

setEmpty :: Set a
setEmpty = []

setIsEmpty :: Eq a => Set a -> Bool
setIsEmpty s = s == []

setSingleton :: a -> Set a
setSingleton x = [x]

setUnionList :: Ord a => [Set a] -> Set a
setUnionList = foldl setUnion setEmpty

type Set a = [a]

-- Other useful function definitions, A.5
first :: (a, b) -> a
first (a, b) = a

second :: (a, b) -> b
second (a, b) = b

-- zipWith already provided by Haskell's Prelude

foldll :: (a -> b -> a) -> a -> [b] -> a
foldll f b []     = b
foldll f b (x:xs) = foldll f (f b x) xs

mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc []     = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')
  where
    (acc1, x') = f acc x
    (acc2, xs') = mapAccuml f acc1 xs
