--Learning Haskell

import Data.List


-- odometer-like permutator
stateList :: Int -> Int -> [[Int]]
stateList n x = buildList n x []
	where 
		buildList 0 _ holder	= [holder]
		buildList n x holder 	= iterateBuildList 0 n x holder
		iterateBuildList k n x holder
			| k > x 			= []
			| otherwise 		= buildList (n - 1) x (holder ++ [k]) ++ (iterateBuildList (k + 1) n x holder) 


-- pretty slow
-- I came up pretty easily with the algorithm but the performance is subpar 
permutate :: Eq a => [a] -> [[a]]
permutate xs = permutate' xs []
	where
		permutate' [] holder 	 = [holder]
		permutate' list holder 	 = iteratePermutate 0 list holder
		iteratePermutate n list holder
			| n == (length list) = []
			| otherwise 		 = let x = list !! n in (permutate' (delete x list) (holder ++ [x]) ++ (iteratePermutate (n + 1) list holder))

concatena ([], []) 	= []
concatena ([], (y:ys)) = y : concatena ([], ys)
concatena ((x:xs), ys) = x : concatena (xs, ys)

uniao ([],[]) = []
uniao (xs, []) = xs ++ uniao([], [])
uniao (xs, (y:ys))
	| y `elem` xs = uniao (xs, ys)
	| otherwise = uniao (xs, ys) ++ [y]
--uniao (xs, ys) = xs ++ uniao (xs, ys)