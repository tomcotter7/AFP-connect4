G52AFP Coursework 1 - Connect Four Game
   
Your full name(s) - Thomas Cotter, Ray Garner
Your full email address(es) - psytc8@nottingham.ac.uk, psyrg4@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.List

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 2
>
> depth :: Int
> depth = 6

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
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

hasRow returns true if all points in a specified row are occupied by a specified player
and false otherwise:

> hasRow :: Player -> Row -> Bool
> hasRow p ps = all (==p) ps

hasWon returns whether a specified player has won based on the contents of the specified board:
doesn't check for diagonals yet

> hasWon :: Player -> Board -> Bool
> hasWon p rs = any (hasRow p) (getAllSubRows rs) ||
>               any (hasRow p) (getAllSubRows (transpose rs))

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
> getDiag n (r:rs) = (r !! n) : (getDiag (n+1) rs)

----------------------------------------------------------------------
