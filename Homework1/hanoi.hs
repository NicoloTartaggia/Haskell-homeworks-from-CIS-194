type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 start end _ = [(start, end)]
hanoi n start end temp = hanoi (n-1) start temp end ++
                         hanoi 1 start end temp ++
                         hanoi (n-1) temp end start