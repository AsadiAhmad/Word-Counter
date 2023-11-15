-- Ahmad Asadi 99463107 --
import Prelude 
    hiding ( Maybe (..)
        ,foldr, filter, insert, sum, length, reverse
        , first, last, lookup, elem, replicate, Monoid)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

filter  :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs) 
 = if p x then x : filter p xs
          else filter p xs

mapReduce
        :: Ord k2
        => (k1 -> [(k2, a)])     -- The mapper function.
        -> (a -> a -> a)         -- The reducer function.
        -> [k1]                  -- Input list
        -> [(k2, a)]             -- Output result.

mapReduce f g xs
 = let  ms      = concat  (map f xs)
        ss      = shuffle ms
   in   map (\(k, ys) -> (k, reduce g ys)) ss

shuffle :: Eq k => [(k, v)] -> [(k, [v])]
shuffle [] = []
shuffle ((k1, v1) : rest)
 = let  match   = map snd (filter (\(k, v) -> k == k1) rest)
        nope    = filter (\(k, v) -> k /= k1) rest
   in   (k1, (v1 : match)) : shuffle nope

reduce :: (a -> a -> a) -> [a] -> a
reduce f []       = error "reduce: no elements"
reduce f (x : xs) = foldr f x xs

wordcount :: [String] -> [(String, Int)]
wordcount xs = mapReduce (\c -> [(c, 1)]) (\x y -> x + y) xs

add :: Int -> Int -> Int
add x y = x + y

main :: IO()
main = do 
   print(wordcount ["ahmad", "hello", "hj", "ahmad"])