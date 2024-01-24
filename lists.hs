example = ["alfred", "Alfred", "Isac", "Erik", "Zlatan", "Ronaldo", "Zendaya", "Timothy Chamalet"]


my_filter :: (a->Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter predicate (x:xs) 
    | predicate x = [x] ++ my_filter predicate xs
    | otherwise = filter predicate xs

-- Sorts the provided List ascendingly, using a bubble-like sort. 
my_sort :: (Ord a) => [a] -> [a]
my_sort arr = predicateSort (\x y -> x>y) arr

bubble :: (Ord a)=> (a->a->Bool) -> [a] -> [a]
bubble _ [] = []
bubble predicate (a:(b:xs)) 
    | predicate a b = [b] ++ bubble predicate ([a] ++ xs)
    | otherwise = [a] ++ bubble predicate ([b] ++ xs)
bubble predicate xs
    | predicate (head xs) (last xs) = reverse xs
    | otherwise = xs

predicateSort :: (Ord a )=> (a->a->Bool) -> [a]-> [a]
predicateSort pred arr
    | (length arr) > 1 = (predicateSort pred (init bubbled)) ++ [last bubbled]
    | otherwise = arr
    where
        bubbled = (bubble pred arr)


