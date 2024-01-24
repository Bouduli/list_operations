
my_filter :: (a->Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter predicate (x:xs) 
    | predicate x = [x] ++ my_filter predicate xs
    | otherwise = filter predicate xs


-- If predicate returns true, then A and B shall swap places.
my_sort :: (Ord a, Num a) => (a->b->Bool) -> [a] -> [a]
my_sort pred arr 
    | (length arr) > 1 = my_sort pred (init bubbled) ++ [last bubbled]
    | otherwise = arr
    where 
        bubbled = (bubble pred arr)
bubble :: (Ord a, Num a)=> (a->b->Bool) -> [a] -> [a]
bubble _ [] = []
bubble predicate (a:(b:xs)) 
    | a > b = [b] ++ bubble predicate ([a] ++ xs)
    | otherwise = [a] ++ bubble predicate ([b] ++ xs)
bubble predicate xs
    | (head xs) > (last xs) = reverse xs
    | otherwise = xs
