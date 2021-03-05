G52AFP Coursework 1 - Connect Four Game
   
Your full name(s) - Thomas Cotter, Ray Garner
Your full email address(es) - psytc8@nottingham.ac.uk, psyrg4@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.List
> import Data.Char
> import System.IO.Unsafe
> import System.Random

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

> first :: Player
> first = O

blank returns a board of rows height and cols width filled entirely with player type B
this is used to initialise the board at the start of a game

> blank :: Board
> blank = replicate rows (replicate cols B)

full fills the entire board with player type X and was used for debugging

> full :: Board
> full = replicate rows (replicate cols X)

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

> data Tree a = Node a [Tree a]
>               deriving (Show)

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
>           pvc blank first


the pvp functions run a game which can be played by two humans against each other

> pvp :: Board -> Player -> IO ()
> pvp bs p = do
>           showBoard bs
>           pvp' bs p

> pvp' :: Board -> Player -> IO ()
> pvp' bs p | hasWon (alt p) bs = printWinner (alt p)
>           | isDraw bs = putStrLn "Draw!"
>           | otherwise =
>                do c <- getCol bs
>                   pvp (move p c bs) (alt p)

the pvc function runs a game where a human player can play against the computer player

> pvc :: Board -> Player -> IO ()
> pvc b p = do showBoard b 
>              c <- getCol b
>              let b' = move p c b              
>              showBoard b'
>              if gameFinished p b' then
>                  printFinished p b'
>              else
>                  do let b'' = bestmove b' (alt p)
>                     if gameFinished (alt p) b'' then
>                         do showBoard b''
>                            printFinished (alt p) b''
>                     else
>                         pvc b'' p

getCol returns an Integer based on the user input

> getCol :: Board -> IO Int
> getCol bs = do
>            xs <- getLine
>            if xs /= [] && all isDigit xs then
>               do
>                   let c = read xs
>                   if valid c bs then
>                       return c
>                   else
>                       do putStrLn "Invalid"
>                          getCol bs
>            else
>               do putStrLn "Invalid"
>                  getCol bs

printFinished is called when the game ends and it detects whether the game was won by either
player or whether it was a draw

> printFinished :: Player -> Board -> IO ()
> printFinished p b | hasWon p b = printWinner p
>                   | otherwise = printDraw

printWinner formats a string based on a given player to produce a notification saying that that player has won

> printWinner :: Player -> IO ()
> printWinner p = putStr ("Player " ++ show p ++ " won!\n")

printDraw simply outputs a notification saying that the game was a draw

> printDraw :: IO ()
> printDraw = putStrLn("Draw!")

gameFinished detects whether the game has ended for a given board

> gameFinished :: Player -> Board -> Bool
> gameFinished p b = hasWon p b || isDraw b

alt is passed a player and returns the opposite player

> alt :: Player -> Player
> alt O = X
> alt X = O
> alt B = B

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


returns true is there is no possible moves left on the board.

> isDraw :: Board -> Bool
> isDraw bs = all (==True) (map (all (/=B)) bs)

returns true if the move is valid.

> valid :: Int -> Board -> Bool
> valid c (b:bs) = c <= cols && y == B
>                  where
>                   (_,y:ys) = splitAt (c - 1) b

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

moves returns a list of boards which can be reached by making one valid move

> moves :: Board -> Player -> [Board]
> moves bs p = [ move p c bs | c <- [1..cols], valid c bs]

gametree builds a tree of boards to a given depth from a given root board
each node has a board and an integer representing the depth of this node

> gametree :: Int -> Board -> Player -> Tree (Board,Int)
> gametree 0 bs p = Node (bs,depth) []
> gametree d bs p | evalboard bs == B = Node (bs,depth-d) [gametree (d-1) b (alt p) | b <- moves bs p]
>                 | otherwise = Node (bs,depth-d) []

minimax attaches an evaluation to each board in the tree
based on whether the board at each leaf has been won by X or O or if it is a draw/incomplete game
the evaluations then propagate their way up the tree to the current board
the depth of the leaf boards will also propagate up the tree so that the ammount of moves required to reach
that end board can be taken into account (less moves to a win is better, more moves to a loss is better)

> minimax :: Player -> Tree (Board,Int) -> Tree (Board,Int,Player)
> minimax p (Node (b,d) []) = Node (b,d,evalboard b) []
> minimax p (Node (b,d) st) = Node (b,depthf depths,besteval) st'
>                         where
>                             st'   = map (minimax (alt p)) st
>                             evals = [e | Node (_,_,e) _ <- st']
>                             depths = [d' | Node (_,d',e') _ <- st', e'==besteval]
>                             pref  = if p==X then maximum else minimum
>                             besteval = pref evals
>                             depthf = if besteval==p then minimum else maximum

evalboard returns the status of a board:
- X has won = X
- O has won = O
- game not over yet = B
- game is a draw = B

> evalboard :: Board -> Player
> evalboard b | hasWon X b = X 
>             | hasWon O b = O
>             | otherwise = B

bestmove returns the optimal move a player can make 
in a given position based on a gametree with evaluations

> bestmove :: Board -> Player -> Board
> bestmove b p = ms !! (randomNum (length ms))
>                where
>                   t  = gametree depth b p
>                   Node (_,d,eval) st = minimax p t
>                   ms = [b' | Node (b',d',p') _ <- st, p' == eval, d'==d]

randomNum generetes a random integer between 0 and n

> randomNum :: Int -> Int
> randomNum n = unsafePerformIO (randomRIO (0,n-1))

----------------------------------------------------------------------
