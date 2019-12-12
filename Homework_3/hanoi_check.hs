module Hanoi_check where
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

-- conversion from string to int
-- "a" = 10, "b" = 11, "c"=12, "d"=13
-- `rem` 10 is necessary to get the correct index of a sublist of a configuration
getIndex :: String -> Int
getIndex (x:_) = (digitToInt x) `rem` 10

-- check if it is possible to move a disc from peg 'from' to peg 'to'
checkHeads :: (Int,Int) -> Config -> Bool
checkHeads (from,to) conf = head (conf !! from) > head (conf !! to)
                    
-- checking of:
-- 1) if we are moving from an empty peg 
-- 2) we are inserting to a non empty peg combined to checkHeads result
checkPegs :: (Int,Int) -> Config -> Maybe Bool
checkPegs (from,to) conf = case (null (conf !! from) || not (null (conf !! to)) && checkHeads (from,to) conf) of
                                       True -> Nothing
                                       False -> Just True

-- update of configuration after a valid move
updateConf:: (Int,Int) -> [Int] -> [Int] -> Config -> Config
updateConf (from,to) sc tc conf = take to firstEdit ++ [tc] ++ drop (to + 1) firstEdit
     where firstEdit = take from conf ++ [sc] ++ drop (from + 1) conf 

-- function which applies a move to a configuration calling the auxiliary functions
checkMove :: (Move) -> Config -> Maybe Config
checkMove (from,to) conf = case (checkPegs (indexFrom,indexTo) conf) of
                                   Nothing -> Nothing
                                   Just True -> Just (updateConf (indexFrom,indexTo) updateStartingCol updateTargetCol conf)
                         where indexFrom = getIndex(from) 
                               indexTo = getIndex(to)
                               removedElem = head (conf !! indexFrom) 
                               updateStartingCol = tail (conf !! indexFrom) 
                               updateTargetCol = [removedElem] ++ (conf !! indexTo)

-- main recursive function which checks moves list and returns the right result 
check :: [Move] -> (Int, Config) -> ((Report, [Move]),(Int, Config))
check ([]) (moves,conf) = ((Ok, []),(moves, conf))
check ((from,to):ms) (moves,conf) = case (checkMove (from,to) conf) of 
                                               Nothing -> ((Bad, [(from,to)]),(moves,conf))
                                               Just newConf -> check ms (moves + 1, newConf)