import Data.Char

type Peg = String
type Move = (Peg, Peg)

triangular :: Int -> Int
triangular k = floor (fromIntegral (k * (k+1)) / 2)

allTri:: [(Int, Int)]
allTri = map (\x -> (x,triangular x)) [1..]

pickx :: Int -> [(Int,Int)] -> Int
pickx n ((a,b):l) = if b < n then pickx n l else a

hanoi3 :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 _ _ _ = []
hanoi3 n a b c = hanoi3 (n-1) a c b ++ [(a,b)] ++ hanoi3 (n-1) c b a

hanoi :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ _ = []
hanoi 1 a b _ _ = [(a,b)]
hanoi 2 a b c _ = [(a,c),(a,b),(c,b)]
hanoi 3 a b c d = [(a,c),(a,d),(a,b),(d,b),(c,b)]
hanoi n a b c d = hanoi (n-x) a c b d ++ hanoi3 x a b d ++ hanoi (n-x) c b a d
     where x = pickx n allTri

data Report = Bad | Ok deriving Show
type Config =[[Int]]

getIndex :: String -> Int
getIndex (x:_) = (digitToInt x) `rem` 10

updateConf :: Move -> Config -> Config
updateConf _ [] = []
updateConf (from,to) [(a:as),(bs),(cs),(ds)] | from == "a" && to == "b" = [(as),(a:bs),(cs),(ds)]
updateConf (from,to) [(a:as),(bs),(cs),(ds)] | from == "a" && to == "c" = [(as),(bs),(a:cs),(ds)]
updateConf (from,to) [(a:as),(bs),(cs),(ds)] | from == "a" && to == "d" = [(as),(bs),(cs),(a:ds)]
updateConf (from,to) [(as),(b:bs),(cs),(ds)] | from == "b" && to == "a" = [(b:as),(bs),(cs),(ds)]
updateConf (from,to) [(as),(b:bs),(cs),(ds)] | from == "b" && to == "c" = [(as),(bs),(b:cs),(ds)]
updateConf (from,to) [(as),(b:bs),(cs),(ds)] | from == "b" && to == "d" = [(as),(bs),(cs),(b:ds)]
updateConf (from,to) [(as),(bs),(c:cs),(ds)] | from == "c" && to == "a" = [(c:as),(bs),(cs),(ds)]
updateConf (from,to) [(as),(bs),(c:cs),(ds)] | from == "c" && to == "b" = [(as),(c:bs),(cs),(ds)]
updateConf (from,to) [(as),(bs),(c:cs),(ds)] | from == "c" && to == "d" = [(as),(bs),(cs),(c:ds)]
updateConf (from,to) [(as),(bs),(cs),(d:ds)] | from == "d" && to == "a" = [(d:as),(bs),(cs),(ds)]
updateConf (from,to) [(as),(bs),(cs),(d:ds)] | from == "d" && to == "b" = [(as),(d:bs),(cs),(ds)]
updateConf (from,to) [(as),(bs),(cs),(d:ds)] | from == "d" && to == "c" = [(as),(bs),(d:cs),(ds)]


check :: [Move] -> (Int, Config) -> ((Report, [Move]),(Int, Config))
check ((from,to):ms) (moves,(conf))
                                   | null (conf !! getIndex(from)) = ((Bad, [(from,to)]),(moves,conf)) 
                                   | not (null (conf !! getIndex(to))) && (head (conf !! getIndex(from)) > head (conf !! getIndex(to))) =  ((Bad, [(from,to)]),(moves,conf))
                                   | length ((from,to):ms) == 1 = ((Ok, []),(moves + 1, updateConf (from,to) conf)) 
                                   | otherwise = check ms (moves + 1, (updateConf (from,to) conf))