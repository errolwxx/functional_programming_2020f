-- Name: Wang Xi
-- Student ID: 71875994
-- CNS: t18599xw

main = print $ take 20 $ twin $ filter isprime [1..] 

factors n = filter divisible [1..n]
  where divisible m = n `mod` m == 0

isprime n = (length $ factors n) == 2

twin :: [Int] -> [(Int, Int)]
twin ps = filter intvlEqlsTwo $ diyzip ps (tail ps)
  where intvlEqlsTwo (x, y) = y - x == 2
        diyzip :: [a] -> [b] -> [(a, b)]
        diyzip xs ys = [((head xs), (head ys))] ++ diyzip (tail xs) (tail ys)