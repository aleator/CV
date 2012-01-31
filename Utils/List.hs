{-# LANGUAGE ScopedTypeVariables #-}
module Utils.List where

import Data.List
import Data.Function
import Data.Maybe
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Test.QuickCheck


-- | Group list into indevidual pairs: [1,2,3,4] => [(1,2),(3,4)]. 
--   Works only with even number of elements
pairs [] = []
pairs [x] = error "Non-even list for pair function"
pairs (x:y:xs) = (x,y):pairs xs

-- | Undo pairs function
fromPairs [] = []
fromPairs ((x,y):xs) = x:y:fromPairs xs

prop_pairsFromTo xs = even (length xs) ==> xs == fromPairs (pairs xs)

-- | Group list into pairs: [1,2,3] => [(1,2),(2,3)]. 
--   Works with non null lists

pairs1 x = zip x (tail x)

-- | Undo pairs1 function
fromPairs1 [] = []
fromPairs1 [(x,y)] = [x,y]
fromPairs1 ((x,y):xs) = x:fromPairs1 (xs)

prop_pairsFromTo1 xs = length xs > 1 ==> xs == fromPairs1 (pairs1 xs)

crease op = map (uncurry op) . pairs1
creaseM op = sequence . (crease op)

ranks f xs = map fst $ rankBy f xs

rankBy f xs = map (\(rank,(orig,val)) -> (rank,val))
              . sortBy (compare`on`(fst.snd))
              . zip [1..] 
              . sortBy (f`on`snd) 
              . zip [1..] 
              $ xs

clusterBy :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy f = M.elems . M.map reverse . M.fromListWith (++)
            . map (f &&& return)


groupItems b a items = map ( (b . head) &&& map a) 
                       . groupBy ((==)`on` b)
                       . sortBy (comparing b) $ items 

-- Assoc-list lookup with default value
lookupDef d a lst = fromMaybe d $ lookup a lst

-- get all consecutive pairs of a list: 
--pairings "kissa"
-- => [('k','i'),('i','s'),('s','s'),('s','a')]

pairings [] = []
pairings [x,y] = [(x,y)]
pairings (x:y:ys) = (x,y):pairings (y:ys)

-- Perform an operation for each in lst
forEach fun lst = unfoldr op ([],lst)
    where
     op (start,[]) = Nothing
     op (start,a:as) = Just (start++(fun a):as
                            ,(start++[a],as)) 

forPairs fun lst lst2 = map (map fst) 
                         $ forEach (\(a,b)->(fun a b,b))
                         $ zip lst lst2

-- 
replicateList n l = concat $ replicate n l
--

concatZipNub (a:as) (b:bs) 
    | a == b = a:concatZipNub as bs
    | a /= b = a:b:concatZipNub as bs                    
concatZipNub [] _ = []
concatZipNub _ [] = []

histogram binWidth values = (map len grouped)
    where
     len x = (snap (head x), fromIntegral (length x)) 
     min = minimum values
     max = maximum values
     grouped = group sorted
     sorted = sort $ map snap values
     snap x = binWidth*(fromIntegral $ floor (x/binWidth))

binList binWidth op ivs = zip bins (map op values) 
    where
     values = map (map snd) grouped
     bins = map (fst.head) grouped 
     grouped = groupBy (\(a,_) (b,_) -> a == b ) sorted
     sorted = sortBy (comparing fst) $ map snapIndex ivs
     snapIndex (i,v) = (binWidth*(i`div`binWidth),v)
     


-- Map numeric list so it becomes zeromean
zeroMean lst = map (\x -> x - mean) lst 
    where mean = average lst

-- Take n best elements according to fitnesses

takeNAccordingTo n (fitnesses,elements) = 
                take n
              $ sortBy (comparing fst)
              $ zip fitnesses elements

-- Zip two lists by selection function
select c = zipWith (\a b -> if c a b then a else b)

-- Take half

takeHalf lst = take (length lst `div` 2) lst

splitToNParts n lst | n <= 0    = error "splitToNParts n <= 0"
                    | otherwise = takeLengths (lengths (length lst) n) lst
        where
        lengths len n = zipWith (+) (replicate n (len`div`n)) (replicate (len`mod`n) 1++repeat 0)

prop_splitEq n xs = n>0 ==> concat (splitToNParts n xs) == xs
prop_splitLen n xs = n>0 && n<= (length xs) ==> length (splitToNParts n xs) == n

-- Count elements that match predicate p
count p = foldl (\sum i -> if p i then sum+1 else sum) 0 

-- Count frequencies of elements in list
frequencies lst = map (\x -> (head x,genericLength x)) $ group $ sort lst
normalizeFrequencies ls = map (\(a,b) -> (a,b/sum (map snd ls))) ls
-- Count average of list
average s = sum s / (genericLength $ s) 

-- Take n smallest given op
smallestBy  op n lst = smallestBy' op n lst [] 
smallestBy' op n [] o = o
smallestBy' op n (i:input) [] = smallestBy' op n input [i]
smallestBy' op n (i:input) output@(o:os) 
     = smallestBy' op n input (take n $ insertBy op i output)
-- (sloppily) Count median of list
median s | odd len = sorted !! middle
         | otherwise = ((sorted !! middle) + 
                       (sorted !! (middle -1))) / 2
    where
     middle = len `div` 2
     sorted = sort s
     len = length s

takeTail n lst = reverse $ take n $ reverse lst

-- Count standard deviation of a list 
stdDev l = sqrt (sum (map (\x -> (x - avg)^2) l)  
                 / genericLength l)
        where avg = average l

-- Transform a list so that nth element is sum of n first elements
cumulate [] = []
cumulate values = tail $ scanl (+) 0 values

schwartzianTransform :: (Ord a,Ord b) => (a -> b) -> [a] -> [a]
schwartzianTransform f = map snd . sort . map (\x -> (f x, x))

sortVia f = map snd . sortBy cmp . map (\x -> (f x , x))
    where cmp (a1,a2) (b1,b2) = compare a1 b1

comparing p a b = compare (p a) (p b)

-- Pick element that has majority in the list
majority lst = head $ maximumBy (comparing length) $ group $ sort lst

-- Get all possible k-sized neighbourhoods in the list
getKNeighbourhoods k p = get (length p) pknot
    where 
        pknot = p++pknot
        get 0 p = []
        get i p = take k p:get (i-1) (tail p)

prop_headIdentical_KN n xs = 1 <= n && length xs >= 1 ==>
                    map head (getKNeighbourhoods n xs)
                    ==
                    xs

-- Split a list to `l` length pieces.
splitToLength l lst = unfoldr split lst
    where
     split [] = Nothing
     split lst = Just (take l lst, drop l lst) 

-- Take n pieces of given lengths
--takeLengths :: [Int] -> [a] -> [[a]]
takeLengths [] lst = []
takeLengths (l:ls) lst = take l lst:takeLengths ls (drop l lst) 

prop_takeLen ls xs = all (>=0) ls &&  sum ls < length xs ==> length (takeLengths ls xs) == length ls
prop_takeLens ls xs = all (>=0) ls &&  sum ls < length xs ==> map length (takeLengths ls xs) == ls

-- From LicencedPreludeExts (hawiki)
splitBy :: (a->Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list =  first : splitBy f (dropWhile f rest)
   where
     (first, rest) = break f list

--splitBetween :: ((a,a) -> Bool) -> [a] -> [[a]]
splitBetween c acc [] = [reverse acc] 
splitBetween c acc [a] = [reverse $ a:acc] 
splitBetween c acc (a:b:cs) | c a b = (reverse $ a:acc):splitBetween c [] (b:cs)
                            | otherwise = splitBetween c (a:acc) (b:cs)

-- split list into subsets matching predicate
tear op l = (filter (not.op) l, filter op l)

swapEverywhere a b = concat $ zipWith merge (inits a) (tails a)
    where
     merge i [] = []
     merge i (t:ts) = map (\x -> i++[x]++ts) b


takeWhile2 op lst = reverse $ tw op [head lst] (tail lst)
 where
    tw _  l [] = []
    tw op l (x:xs) = if op (head l) x 
                              then tw op (x:l) xs
                              else l

applyMap val ops = map (\op -> op val) ops 
applyMapM :: (Monad m) => a -> [a -> m b] -> m [b]
applyMapM val ops = mapM (\op -> op val) ops 
changesM :: (Monad m) => [a -> m b] -> a -> m [b]
changesM = flip applyMapM

rollList (a:xs) = xs ++[a]
roll = rollList

mergeList a b = a ++ drop (length a) b

takeWhile1 test [] = []
takeWhile1 test (x:xs) | test x = x:takeWhile1 test xs
                       | otherwise =  [x]


-- Modify each element in list with function that has knowledge of already
-- modified elements
editingMap f l = editingTrav f [] l

editingTrav fun [] l@(x:xs) = editingTrav fun [(fun l x)] xs
editingTrav fun a [] = reverse a
editingTrav fun ss l@(x:xs) = editingTrav fun
                                         (fun (reverse ss++l) x:ss)
                                         xs


-- Rotations of list
rotate (x:xs) = xs++[x]
cycles x = take (length x) $ iterate rotate x
