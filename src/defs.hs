doubleMe x = x + x
--concat two lists (or strings, which are lists)
concatList a b = a ++ b
--prepend "a" item to "l" list
prependToList a l = a:l
--get list "l" item contained in "i" index
getItem i l = l !! i
--uses list ranges to get the first X elements multiple of 7
getFirstXMultipleOfSeven n = take n [7, 14..]
--uses list comprehension to return a squared list
squaredList l = [x^2 | x <- l]
--uses list comprehension to return a squared list multiple of 7
squaredListMulSeven l = [x^2 | x <- l, x `mod` 7 == 0]
--uses pattern matching to sum a list
length' l = sum [1 | _ <- l]

removeUppercase s = [x | x <- s, x `elem` ['a'..'z']]

factorial' 0 = 1
factorial' n = n * factorial' (n-1)

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (s:xs) = quicksort' [x|x <- xs,x < s]
    ++ [s] ++
    quicksort' [x|x <- xs,x >= s]

max' [a] = a
max' (a : t) = if (a > (max' t)) then a
    else (max' t)