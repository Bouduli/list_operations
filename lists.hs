
my_filter :: (a->Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter predicate (x:xs) 
    | predicate x = [x] ++ my_filter predicate xs
    | otherwise = filter predicate xs

-- Sorts the provided List ascendingly, using a bubble-like sort. 
my_sort :: (Ord a) => [a] -> [a]
my_sort arr
    | (length arr) > 1 = (my_sort (init bubbled)) ++ [last bubbled]
    | otherwise = arr
    where
        -- If predicate returns true, then elements x and y swap places
        pred = (\x y -> y<x)
        bubbled = (bubble pred arr)
bubble :: (Ord a)=> (a->a->Bool) -> [a] -> [a]
bubble _ [] = []
bubble predicate (a:(b:xs)) 
    | predicate a b = [b] ++ bubble predicate ([a] ++ xs)
    | otherwise = [a] ++ bubble predicate ([b] ++ xs)
bubble predicate xs
    | predicate (head xs) (last xs) = reverse xs
    | otherwise = xs
