-- Name: Wang Xi 
-- Stuent ID: 71875994
-- CNS: t18599xw@sfc.keio.ac.jp

main = do cs <- getContents
          putStr $ concatMap sfc cs

sfc :: Char -> [Char]
-- sfc cs = if cs == 'S' then "Shonan " 
--                       else if cs == 'F' then "Fujisawa " 
--                                         else if cs == 'C' then "Campus " 
--                                                           else [cs] 
sfc 'S' = "Shonan"
sfc 'F' = "Fujisawa"
sfc 'C' = "Campus" 
sfc xs = [xs] >>= (\xs -> not `elem` "SFC")
