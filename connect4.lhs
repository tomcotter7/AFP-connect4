G52AFP Coursework 1 - Connect Four Game
   
Your full name(s) - Thomas Cotter, Ray Garner
Your full email address(es) - psytc8@nottingham.ac.uk, psyrg4@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.List
> import Data.Char

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 3
>
> cols :: Int
> cols = 3
>
> win :: Int
> win = 3
>
> depth :: Int
> depth = 6

> first :: Player
> first = O

> blank :: Board
> blank = [[B,B,B],[B,B,B],[B,B,B]]

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['1'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

Below is the main game loop code, Player O starts and input is read and a move is made.

> main :: IO ()
> main = do 
>           putStrLn "Player O goes first"
>           run blank first


> run :: Board -> Player -> IO ()
> run bs p = do
>           showBoard bs
>           run' bs p

> run' :: Board -> Player -> IO ()
> run' bs p | hasWon (next p) bs = printWinner (next p)
>           | otherwise =
>                do c <- getCol
>                   run (move p c bs) (next p)

getCol returns an Integer based on the user input

> getCol :: IO Int
> getCol = do
>            xs <- getLine
>            if xs /= [] && all isDigit xs then
>               do
>                   let c = read xs
>                   if c <= cols then
>                       return c
>                   else
>                       do putStrLn "Invalid"
>                          getCol
>            else
>               do putStrLn "Invalid"
>                  getCol

> printWinner :: Player -> IO ()
> printWinner p = putStr ("Player " ++ show p ++ " won!\n")

Utility Functions

Turn returns which players turn it is. Will return O if the game hasn't started as O goes first.

> next :: Player -> Player
> next O = X
> next X = O
> next B = B


> turn :: Board -> Player
> turn xs | o <= x = O
>         | otherwise = X
>           where
>               o = noOf O xs
>               x = noOf X xs 

noOf counts the noOf times a player has played a piece.

> noOf :: Player -> Board -> Int
> noOf p = length . concat . map (filter (==p))

move returns a new board after the move has been made. Input a player, column and board

> move :: Player -> Int -> Board -> Board
> move p col xs = reverse (makeMove p col (reverse xs))

makeMove makes the moves on the reversed board and returns the updated reversed board

> makeMove :: Player -> Int -> Board -> Board
> makeMove p col [] = []
> makeMove p col (b:bs) | y == B = (x ++ p : ys) : bs
>                       | otherwise = b : makeMove p col bs
>                         where
>                           (x,y:ys) = splitAt (col - 1) b




hasRow returns true if all points in a specified row are occupied by a specified player
and false otherwise:

> hasRow :: Player -> Row -> Bool
> hasRow p ps = all (==p) ps

hasWon returns whether a specified player has won based on the contents of the specified board:

> hasWon :: Player -> Board -> Bool
> hasWon _ [] = False
> hasWon p rs = any (hasRow p) (getAllSubRows rs) ||
>               any (hasRow p) (getAllSubRows (transpose rs)) ||
>               any (hasRow p) (getAllSubRows (getAllDiags rs))

getAllSubRows returns all horizontal sub rows (contiguous) of length 'win' from a board:

> getAllSubRows :: Board -> [Row]
> getAllSubRows [] = []
> getAllSubRows (x:xs) = getSubRows x ++ getAllSubRows xs

getSubRows returns the sub rows (contiguous) of length 'win' from one row:

> getSubRows :: Row -> [Row]
> getSubRows rs | length rs < win = []
>               | otherwise = (take win rs) : getSubRows (drop 1 rs)

returns a diagonal line down and right in the matrix from an index of the top row

> getDiag :: Int -> Board -> Row
> getDiag _ [] = []
> getDiag n (r:rs) | n < length r = (r !! n) : (getDiag (n+1) rs)
>                  | otherwise = []

getColDiags returns a list of diagonals stemming down and right from each point on the top row

> getColDiags :: Int -> Board -> [Row]
> getColDiags _ [] = []
> getColDiags n rs | n < length (head rs) = getDiag n rs : (getColDiags (n+1) rs)
>                  | otherwise = []

getAllDiags returns all diagonals on the board as a list of rows with no duplicates

> getAllDiags :: Board -> [Row]
> getAllDiags rs = d ++ t ++ r ++ tr
>                  where
>                      d = getColDiags 0 rs
>                      t = getColDiags 1 (transpose rs)
>                      r = getColDiags 0 reverseboard
>                      tr = getColDiags 1 (transpose reverseboard)
>                      reverseboard = reverse rs

----------------------------------------------------------------------
