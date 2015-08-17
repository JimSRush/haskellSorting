--Some sorting algorithm imlementations in haskell

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort(x:xs) = quickSort sortLeft ++ [x] ++ quickSort sortRight
	where 
		sortLeft = [a | a <- xs, a <= x] 
		sortRight = [a | a <- xs, a > x]


--quicksort with a helper method, for fun I guess
sort' :: (Ord a) => [a] -> [a]
sort' [] = [] 
sort' [a] = [a]
sort' (c:cs) = sort'' [c] cs

sort'' :: (Ord a) => [a] -> [a] -> [a]
sort'' d [] = d
sort'' d (c:cs) = sort'' (lessThan ++ [c] ++ greaterThan) cs
	where
		lessThan = [y | y <- d, y <= c];
		greaterThan = [y | y <-d, y>c]