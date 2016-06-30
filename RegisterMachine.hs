data Instr = MINUS Int Int Int | PLUS Int Int | HALT

instance Show Instr where
   show (MINUS a b c) = "(R" ++ show a ++ "- -> " ++ "L" ++ show b ++ ", L" ++ show c ++ ")"
   show (PLUS a b) = "(R" ++ show a ++ "+ -> " ++ "L" ++ show b ++ ")"
   show (HALT) = "(HALT)"

-- Encoding instructions:

encode :: [Instr] -> Int
encode xs = encodeList (map encodeInstr xs)

encodeInstr :: Instr -> Int
encodeInstr (PLUS reg a) = encodeDoublePair (2 * reg, a)
encodeInstr (MINUS reg a b) = encodeDoublePair ((2 * reg) + 1, encodeSinglePair (a, b))
encodeInstr HALT = 0

encodeList :: [Int] -> Int
encodeList (x : xs) = encodeDoublePair (x, encodeList xs)
encodeList [] = 0

encodeDoublePair :: (Int, Int) -> Int
encodeDoublePair (a, b) = (2 ^ a) * ((2 * b) + 1)

encodeSinglePair :: (Int, Int) -> Int
encodeSinglePair (a, b) = (2 ^ a) * ((2 * b) + 1) - 1

-- Decoding instructions:

decode :: Int -> [Instr]
decode n = map decodeInstr xs
	where
		xs = decodeList n

decodeInstr :: Int -> Instr
decodeInstr n 
	|n == 0 = HALT
	|x `mod` 2 == 0 = (PLUS (x `div` 2) y)
	|True = (MINUS ((x - 1) `div` 2) a b)
	where
		(x, y) = decodeDoublePair n
		(a, b) = decodeSinglePair y

decodeList :: Int -> [Int]
decodeList n
	|n == 0 = []
	|True = x : decodeList y
		where
			(x, y) = decodeDoublePair n

decodeDoublePair :: Int -> (Int, Int)
decodeDoublePair n = (x, y)
	where
		x = log2 n
		y = ((n `div` (2 ^ x)) - 1) `div` 2

decodeSinglePair :: Int -> (Int, Int)
decodeSinglePair n = (x, y)
	where
		x = log2 (n + 1)
		y = (((n + 1) `div` (2 ^ x)) - 1) `div` 2

log2 :: Int -> Int
log2 n
	|(n `mod` 2 == 1) = 0
	|True = 1 + log2 (quot n 2)
