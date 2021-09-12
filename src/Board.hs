module Board (
    Square(..),
    Board,
    emptyBoard,    -- Board
    writeSquare,   -- Int -> Square -> Board -> Board
    readSquare,    -- Int -> Board -> Square
    isRowFull,     -- Square -> Board -> Bool
    isColFull,     -- Square -> Board -> Bool
    isDiagFull,    -- Square -> Board -> Bool
    isSquareEmpty, -- Int -> Board -> Bool
    isFull         -- Board -> Bool
) where

data Square = Empty | X | O deriving (Eq)
instance Show Square where
    show Empty = " "
    show X = "X"
    show O = "O"

type Row = (Square, Square, Square)

-- A representation of a tictactoe board. Spaces are indexed as follows:
--    |   |   
--  0 | 1 | 2 
--    |   |   
-- -----------
--    |   |   
--  3 | 4 | 5 
--    |   |   
-- -----------
--    |   |   
--  6 | 7 | 8 
--    |   |   
newtype Board = Board (Row, Row, Row)
instance Show Board where
    show (Board ((s11, s12, s13),
                 (s21, s22, s23),
                 (s31, s32, s33))) =
        "   |   |   \n" ++
        " " ++ show s11 ++ " | " ++ show s12 ++ " | " ++ show s13 ++ " \n" ++
        "   |   |   \n" ++
        "-----------\n" ++
        "   |   |   \n" ++
        " " ++ show s21 ++ " | " ++ show s22 ++ " | " ++ show s23 ++ " \n" ++
        "   |   |   \n" ++
        "-----------\n" ++
        "   |   |   \n" ++
        " " ++ show s31 ++ " | " ++ show s32 ++ " | " ++ show s33 ++ " \n" ++
        "   |   |   \n"

emptyBoard :: Board
emptyBoard =
    Board ((Empty, Empty, Empty),
           (Empty, Empty, Empty),
           (Empty, Empty, Empty))

-- Writes the given Square to the given space index.
-- Must satisfy 0 <= n <= 8
writeSquare :: Int -> Square -> Board -> Board
writeSquare n sqr (Board (r1, r2, r3))
    | (n >= 0) && (n < 3) = Board (insertInRow cIdx sqr r1, r2, r3)
    | (n >= 3) && (n < 6) = Board (r1, (insertInRow cIdx sqr r2), r3)
    | (n >= 6) && (n < 9) = Board (r1, r2, (insertInRow cIdx sqr r3))
    | otherwise = error "Invalid input in function writeSquare! (0 <= n <= 8 required)"
    where
        cIdx = n `mod` 3

-- Writes the given Square to the correct space in a row.
-- Must satisfy 0 <= n <= 2
insertInRow :: Int -> Square -> Row -> Row
insertInRow n sqr (s1, s2, s3)
    | n == 0 = (sqr, s2, s3)
    | n == 1 = (s1, sqr, s3)
    | n == 2 = (s1, s2, sqr)
    | otherwise = error "Invalid input to insertInRow! (0 <= n <= 2 required)"

-- Reads the value of the Square at the given space index.
-- Must satisfy 0 <= n <= 8
readSquare :: Int -> Board -> Square
readSquare n (Board (r1, r2, r3))
    | (n >= 0) && (n < 3) = readFromRow cIdx r1
    | (n >= 3) && (n < 6) = readFromRow cIdx r2
    | (n >= 6) && (n < 9) = readFromRow cIdx r3
    | otherwise = error "Invalid input to readSquare! (0 <= n <= 8 required)"
    where
        cIdx = n `mod` 3

-- Writes value of the Square at the given index in a row.
-- Must satisfy 0 <= n <= 2
readFromRow :: Int -> Row -> Square
readFromRow n (s1, s2, s3)
    | n == 0 = s1
    | n == 1 = s2
    | n == 2 = s3
    | otherwise = error "Invalid input to readFromRow! (0 <= n <= 2 required)"

-- Returns True if any row of the given Board is full of the given Square symbol
isRowFull :: Square -> Board -> Bool
isRowFull sqr (Board (r1, r2, r3)) =
    (r1 == (sqr, sqr, sqr)) || (r2 == (sqr, sqr, sqr)) || (r3 == (sqr, sqr, sqr))

-- Returns True if any column of the given Board is full of the given Square symbol
isColFull :: Square -> Board -> Bool
isColFull sqr (Board ((s11, s12, s13),
                      (s21, s22, s23),
                      (s31, s32, s33))) =
    ((s11 == sqr) && (s21 == sqr) && (s31 == sqr)) ||
    ((s12 == sqr) && (s22 == sqr) && (s32 == sqr)) ||
    ((s13 == sqr) && (s23 == sqr) && (s33 == sqr))

-- Returns True if any diagonal of the given Board is full of the given Square symbol
isDiagFull :: Square -> Board -> Bool
isDiagFull sqr (Board ((s11, s12, s13),
                       (s21, s22, s23),
                       (s31, s32, s33))) =
    ((s11 == sqr) && (s22 == sqr) && (s33 == sqr)) ||
    ((s13 == sqr) && (s22 == sqr) && (s31 == sqr))

isSquareEmpty :: Int -> Board -> Bool
isSquareEmpty n board = readSquare n board == Empty

-- Returns True if no squares on the Board are empty.
isFull :: Board -> Bool
isFull (Board (r1, r2, r3)) = fullRow r1 && fullRow r2 && fullRow r3

-- Returns True if no squares in the Row are empty.
fullRow :: Row -> Bool
fullRow (s1, s2, s3) = (s1 /= Empty) && (s2 /= Empty) && (s3 /= Empty)