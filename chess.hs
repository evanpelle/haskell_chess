module Chess
where

import Data.List
import Data.Char
import Data.Maybe
import System.IO


--                             ##           #                    #                  
--     ####                     #     ###   #                    #                  
--      # #  ###  ##   # ##  ###     #  # ####  # ## ## #  ### #### ## #  # ##  ### 
--     ###  #  #   ##  ##   #  #      #    #    ##    # # #     #    # #  ##   #### 
--     # #  #  # ###   #    # #     #  #  #     #    # ## #  # #    # ##  #    #    
--    ####  ###  # #  ##     # #    ###   ###  ##    ## #  ##  ###  ## # ##     ##  
data Color = White | Black | None deriving (Show, Eq)
data Piece = 
    King {getColor :: Color} | Queen  {getColor :: Color} |
    Rook  {getColor :: Color} | Bishop  {getColor :: Color} |
    Knight  {getColor :: Color} | Pawn  {getColor :: Color} |  Empty deriving Eq

type Row = [Piece]

type Board = [Row]

type Position = (Int, Int)
type Direction = (Int, Int) -- (1,0) for right, (-1, 0) for left, (1,1) for down-right diagonal, etc  


type Move = (Position, Position)


middleBoard = take 4 $ repeat $ take 8 $ repeat Empty
blackPawnRow = [take 8 $ repeat $ Pawn Black]
whitePawnRow = [take 8 $ repeat $ Pawn White]

startBoard =
    [[Rook Black, Knight Black, Bishop Black, Queen Black,
    King Black, Bishop Black, Knight Black, Rook Black]]
    ++ blackPawnRow ++ middleBoard ++ whitePawnRow ++
    [[Rook White, Knight White, Bishop White, Queen White,
    King White, Bishop White, Knight White, Rook White]]

instance Show Piece where
    show (King White) = "K"
    show (King Black) = "k"
    show (Queen White) = "Q"
    show (Queen Black) = "q"
    show (Rook White) = "R"
    show (Rook Black) = "r"
    show (Bishop White) = "B"
    show (Bishop Black) = "b"
    show (Knight White) = "N"
    show (Knight Black) = "n"
    show (Pawn White) = "P"
    show (Pawn Black) = "p"
    show Empty = " " 


--     ####                     #    ###         #                         #                  
--      # #  ###  ##   # ##  ###      #   # ## ####  ###  # ##  ##   ### ####  ##   ###  # ## 
--     ###  #  #   ##  ##   #  #     #    ## #  #   ####  ##     ## #     #     #  #  #  ## # 
--     # #  #  # ###   #    # #      #    #  # #    #     #    ###  #  # #     #   #  #  #  # 
--    ####  ###  # #  ##     # #    ###  ## ## ###   ##  ##    # #   ##  ###  ###  ###  ## ## 
--                                                                                            
printRow :: Row -> String
printRow r = "|" ++ (foldl1 (++) $ fmap (\p -> "  " ++ (show p) ++ "  |" ) r) ++ "\n"
    
boardString :: Board -> String
boardString b = rowBreak ++ (foldl1 (++) $ intersperse rowBreak $ fmap printRow b) ++ rowBreak
    where rowBreak = "|-----------------------------------------------|\n"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth index element list = (take index list) ++ [element] ++ (drop (index+1) list)


getPiece :: Position -> Board -> Piece
getPiece (x, y) board = (board !! y) !! x

setPiece :: Position -> Piece -> Board -> Board
setPiece pos piece board = replaceNth (snd pos) newRow board
    where newRow = replaceNth (fst pos) piece (board !! (snd pos))

getPosition :: Piece -> Board -> Maybe Position
getPosition piece board = 
    if maybeIndex == Nothing then
        Nothing
    else
        let index = fromJust maybeIndex in 
            Just (mod index 8, quot index 8) 
    where 
        maybeIndex = elemIndex piece $ concat board

isEmpty :: Position -> Board -> Bool
isEmpty pos board = 
    getPiece pos board == Empty


makeMove :: Move -> Board -> Board
makeMove (start, end) board = let piece = getPiece start board in
    setPiece start Empty $ setPiece end piece board



--                 ##    #    ##                        
--    ## ##         #          #    #  #                
--     # #   ##     #  ##   ###     # ##  ### ## #  ### 
--     ##     ##   #    #  #  #     ## # #  #  # # #### 
--     #    ###    #   #   # #     #  #  #  #  ##  #    
--     #    # #  #### ###   # #    # ##  ###   #    ##  
--                                                      
generateMoves :: Position -> Board -> [Position]
generateMoves _ _ = [(0,0)]


isOnBoard :: Position -> Bool
isOnBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- takes move that a piece would like to move to
isValidPosition :: Board -> Color -> Position -> Bool
isValidPosition board color (x, y) 
        | toPlace == Empty && onBoard = True
        | color /= (getColor toPlace) && onBoard = True
        | otherwise = False
        where
            onBoard = isOnBoard (x,y)
            toPlace = getPiece (x, y) board




--PAWNS
--genLegalPawnMoves :: Position -> Board -> [Position]
--    genLegalPawnMoves = filter isValidPawnMove $ filter isValidPosition generateAllPawnMoves


generateAllPawnMoves :: Position -> Board -> [Position]
generateAllPawnMoves (x, y) board = 
    [(x, y-1), (x, y+1), (x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1,y-1)]

--isValidPawnMove :: Move -> Board -> Bool
--isValidPawnMove ((x1, y1), (x2, y2)) board = 
--    if getColor piece == Black then        
--        if y2 > y1 then

--        else
--            False 
--    else 
--        something else 
--    where
--        piece = getPiece start board


-- KNIGHTS

--genKnightMoves :: Board -> Position -> [Position] 
--genAllKnightMoves (x, y) =
--    filter (isValidPosition board (getColor (x,y)) positions
--        where
--        positions =  map (\list -> (head list, head $ tail list)) list_positions
--            where
--            list_positions = (sequence [[x+2, x-2], [y+1, y-1]]) ++ (sequence [[x+1 , x-1], [y+2, y-2]])


rookMoves :: Color -> Position -> Board -> [Position]
rookMoves color pos board = 
    left ++ right ++ up ++ down
    where
        left = getSeries color pos (-1,0) board
        right = getSeries color pos (1,0) board
        up = getSeries color pos (0,-1) board
        down = getSeries color pos (0,1) board

bishopMoves :: Color -> Position -> Board -> [Position]
bishopMoves color pos board =
    rightDown ++ rightUp ++ leftDown ++ leftUp 
    where
        rightDown = getSeries color pos (1,1) board
        rightUp = getSeries color pos (1,-1) board
        leftDown = getSeries color pos (-1,1) board
        leftUp = getSeries color pos (-1,-1) board

queenMoves :: Color -> Position -> Board -> [Position]
queenMoves color pos board = 
    bMoves ++ rMoves
    where
        bMoves = bishopMoves color pos board
        rMoves = rookMoves color pos board


getSeries :: Color -> Position -> Direction -> Board -> [Position]
getSeries color pos dir board =
    drop 1 $ filterLine color (genLine pos dir) board


filterLine :: Color -> [Position] -> Board -> [Position]
filterLine _ [] _ = []
filterLine color (x:xs) board =
    if (not $ isOnBoard x ) || (not isEmpty && pieceColor == color) then
        []
    else
        if (not isEmpty && pieceColor /= color) then
            [x]
        else 
            x : (filterLine color xs board)
    where
        isEmpty = getPiece x board == Empty      
        pieceColor = getColor $ getPiece x board  

genLine :: Position -> Direction -> [Position]
genLine (x,y) (dx, dy) =
    if isOnBoard (x,y) then
        (x, y) : genLine (x+dx, y+dy) (dx, dy)
    else
        []


--merges two lists to a list of tuples (good for generating list of coordinates)
mergeList :: [a] -> [a] -> [(a,a)]
mergeList [] _ = []
mergeList _ [] = []
mergeList (x1:xs1) (x2:xs2) = (x1, x2) : mergeList xs1 xs2