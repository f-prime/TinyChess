module Main where

data Piece = Pawn
  | Rook
  | Bishop
  | Knight
  | Queen
  | King deriving Show

data Team = Black | White deriving Show
data TeamPiece = Occupied Team Piece | Empty deriving Show

type Position = (Char, Int)
type Board = [TeamPiece]
type PositionString = [Char]

resetBoard :: Board
resetBoard = [ 
  Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn,
  Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black King, Occupied Black Queen, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
  Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn,
  Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White King, Occupied White Queen, Occupied White Bishop, Occupied White Knight, Occupied White Rook ]

validCol :: Char -> Bool
validCol x = x >= 'a' && x <= 'h' 

validRow :: Int -> Bool
validRow row = row >= 0 && row < 7

doesPositionExist :: Position -> Bool
doesPositionExist (col, row) = validCol col && validRow row

positionToArrayIndex :: Position -> Int
positionToArrayIndex (colLabel, row) = 63 - col colLabel - (row - 1) * 8 
  where
    col :: Char -> Int
    col 'a' = 7
    col 'b' = 6
    col 'c' = 5
    col 'd' = 4
    col 'e' = 3
    col 'f' = 2
    col 'g' = 1
    col 'h' = 0

positionStringToPosition :: PositionString -> Position
positionStringToPosition (col:row) = (col, read row::Int) 

positionStringToArrayIndex :: PositionString -> Int
positionStringToArrayIndex ps = positionToArrayIndex $ positionStringToPosition ps

getPieceAtPosition :: PositionString -> Board -> TeamPiece
getPieceAtPosition ps board = board !! positionToArrayIndex (positionStringToPosition ps) 

canMovePiece :: PositionString -> PositionString -> Board -> Bool
canMovePiece from to board = False

movePiece :: PositionString -> PositionString -> Board -> Board
movePiece from to board = if canMovePiece from to board then boardStateAfterMove else board
  where
    boardStateAfterMove = toFromSplitLeft ++ [fromPiece] ++ toFromSplitRight 

    (toFromSplitLeft, _:toFromSplitRight) = splitAt toIndex boardWithoutFrom

    boardWithoutFrom = fromSplitLeft ++ [Empty] ++ fromSplitRight
    (fromSplitLeft, _:fromSplitRight) = splitAt fromIndex board

    fromPiece = getPieceAtPosition from board

    fromIndex = positionStringToArrayIndex from
    toIndex = positionStringToArrayIndex to

pieceToString :: TeamPiece -> [Char]
pieceToString (Occupied Black Pawn) = "Bp" 
pieceToString (Occupied White Pawn) = "Wp" 
pieceToString (Occupied Black Rook) = "Br" 
pieceToString (Occupied White Rook) = "Wr" 
pieceToString (Occupied Black Knight) = "Bn" 
pieceToString (Occupied White Knight) = "Wn" 
pieceToString (Occupied Black Queen) = "Bq" 
pieceToString (Occupied White Queen) = "Wq" 
pieceToString (Occupied Black King) = "Bk" 
pieceToString (Occupied White King) = "Wk" 
pieceToString (Occupied Black Bishop) = "Bb" 
pieceToString (Occupied White Bishop) = "Wb" 
pieceToString Empty = " "

getNewBoard :: IO Board
getNewBoard = return resetBoard

printBoard :: IO Board -> IO ()
printBoard board = pb board 0 
  where
    pb :: IO Board -> Int -> IO ()
    pb b 64 = do 
      putStrLn ""
    pb b i = do 
      board <- b
      putStr $ pieceToString $ head board 
      if (i + 1) `mod` 8 == 0 then putStrLn "" else putStr " "
      pb (return $ tail board) $ i + 1
 
main :: IO ()
main = do
  board <- getNewBoard 
  printBoard $ return board
