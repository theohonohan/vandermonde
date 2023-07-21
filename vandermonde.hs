-- Generalised Vandermonde's identity, in Haskell
--
-- See https://en.wikipedia.org/wiki/Vandermonde%27s_identity#Generalized_Vandermonde's_identity
--
-- code to produce weak compositions borrowed from 
-- http://www-cs-students.stanford.edu/~blynn/haskell/count.html

subsets xs 0 = [[]]
subsets xs k
  | length xs < k = []
  | otherwise     = subsets t k ++ map (h:) (subsets t (k - 1))
  where (h:t) = xs

compositions n k = f <$> subsets [1..n-1] (k-1)
  where f s = zipWith (-) (s ++ [n]) (0:s)

weakCompositions n k = map (+(-1)) <$> compositions (n+k) k

binom :: Int -> Int -> Int
binom = loop 1 1
  where
    loop rn rd _ 0 = rn `div` rd
    loop _  _  0 _ = 0
    loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1)

van1 ns m = sum $ map product (map (zipWith binom ns) (weakCompositions m (length ns)))
van2 ns m = binom (sum ns) m

-- van1 should be equal to van2
