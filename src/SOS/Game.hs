{-# LANGUAGE RecordWildCards #-}
    
module SOS.Game
  ( startGame
  ) where

import Control.Monad
  ( liftM
  , forM_
  , replicateM_
  )
import qualified Data.Map as Map
import qualified Data.List as List       
import Data.Char
  ( toUpper
  , toLower
  , isLetter
  , isDigit
  )
import Safe (at)  
import System.Random (getStdGen, random)

import SOS.Player
  ( Player(playerName, playerScore)
  , createPlayer
  , updatePlayerScore
  )      
                            
type Col = Char
type RowNum = Int
type Width = Int     
type Height = Int
     
type PiecePosition = (RowNum, Col)
type Dimensions = (Width, Height)
type PieceCoords = (Int, Int) -- x,y
type PlayerName = String     

data Players = Players
  { plPlayers :: Map.Map PlayerName Player
  , plPlayerTurn :: PlayerName
  } deriving Show
  
data GameComponents = GameComponents
  { gcBoard   :: Board
  , gcPlayers :: Players
  } deriving Show
  
data Board = Board
  { bPieces :: Map.Map RowNum [Piece]
  , bDimensions :: Dimensions
  , bAvailableSpots :: Int
  , bLatestPieceCoords :: Maybe PieceCoords
  } deriving Show

data Piece =
    S -- ^ Letter S
  | O -- ^ Letter O
  | E -- ^ Empty
  deriving (Show,Eq,Read)      

startGame :: IO ()
startGame = do
  putStrLn "Hi there, Welcome to the SOS game \n"
  putStrLn "Player 1, please enter your name"
  player1Name <- getLine
  putStrLn "\nNow, Player 2, please go ahead and enter your name as well"
  player2Name <- getLine
  putStrLn "\nHow big of a board would you like to play on\ 
            \ (e.g., 3x3, 4x4, 6x6, etc) ?"
  dimensions <- readDimensions
  startPlaying dimensions (createPlayer player1Name, createPlayer player2Name) 
  
updatePlayers :: Player -> Players -> Players
updatePlayers player pl@Players{..} =
  pl {plPlayers = Map.insert (playerName player) player plPlayers}

playingPlayer :: Players -> Player
playingPlayer Players{..} =
  let mplayer = Map.lookup plPlayerTurn plPlayers
  in case mplayer of
    Just player -> player
    _ -> error "Impossible happened: Player is not found"

updatePlayerTurn :: Players -> Players
updatePlayerTurn pl@Players{..} =
  let playerNames = (Map.keys plPlayers) 
  in pl {plPlayerTurn = (head . filter(/= plPlayerTurn)) playerNames}

isGameOver :: Board -> Bool
isGameOver Board{..} = bAvailableSpots == 0                

startPlaying :: Dimensions -> (Player, Player) -> IO ()
startPlaying dims (player1, player2) =
  case initBoard dims of
    Left e -> error $ show e;
    Right board -> do
      putStrLn "\nAnd here begins the game ! \n"
      gen <- getStdGen 
      let players = Players (Map.fromList [(playerName player1, player1),
                            (playerName player2, player2)]) (startingPlayer gen)
      gameLoop $ GameComponents board players  
 where
   startingPlayer gen =
     let starter = fst $ random gen    
     in if starter then playerName player1 else playerName player2
                                     
gameLoop :: GameComponents -> IO ()
gameLoop gc@GameComponents{..} = do
  displayScore gcPlayers
  displayBoard gcBoard
  showPlayerTurn
  moveRes <- placePiece <$> readPiece <*> readPiecePos <*> pure gc
  case moveRes of
    Left err -> (putStrLn $ show err) >> putChar '\n' >> gameLoop gc
    Right ugc@(GameComponents updatedBoard updatedPlayers) -> do
      putChar '\n'
      if isGameOver updatedBoard
       then putStrLn $ outcome updatedPlayers
       else gameLoop ugc
      putChar '\n'     

 where showPlayerTurn = putStrLn $ (++ " > ") (plPlayerTurn gcPlayers) 
         
placePiece :: Piece
           -> PiecePosition
           -> GameComponents
           -> Either MoveErr GameComponents
placePiece piece piecePos GameComponents{..} = 
  let coords = piecePosToCoords piecePos
  in
  case putPiece gcBoard piece coords of
    Right updatedBoard ->
      let numOfNewSeqs = numOfCreatedSequences updatedBoard coords 
      in if numOfNewSeqs > 0
          then let updatedPlayer = updatePlayerScore numOfNewSeqs
                                    (playingPlayer gcPlayers)
                   updatedPlayers = updatePlayers updatedPlayer gcPlayers
               in return $ GameComponents updatedBoard updatedPlayers 
          else
            return $ GameComponents updatedBoard (updatePlayerTurn gcPlayers)   
    Left err -> Left err 
 where
   putPiece :: Board -> Piece -> PieceCoords -> Either MoveErr Board
   putPiece board piece' coords@(rowNo, colNo) =
     if isWithinBoard board coords
      then let (Just row) = rowNoToRow rowNo board
           in case row `at` colNo of
                (E) -> let newRow = replaceAtIdx row colNo piece'
                       in Right $ Board (Map.insert rowNo newRow $ bPieces board)
                                        (bDimensions board)
                                        (subtract 1 $ bAvailableSpots board)
                                        (Just coords)
                _ -> spotIsOccupied  
      else spotDoesNotExist  
    where spotIsOccupied = Left SpotTaken  
          spotDoesNotExist = Left SpotNotExists

data MoveErr
  = SpotTaken
  | SpotNotExists

instance Show MoveErr where
  show SpotTaken = "Sorry, that spot is already occupied."
  show SpotNotExists = "Such a spot does not exist on the board"
  
readPiece :: IO Piece
readPiece = do          
  pieceStr <- filter(/=' ') `liftM` getLine
  if not . null $ pieceStr
   then let capitializedPiece = [toUpper . head $ pieceStr]
        in case toPiece capitializedPiece of
             Right p -> return p
             Left err -> print err >> readPiece
   else readPiece

data WrongPieceErr = WrongPiece String

instance Show WrongPieceErr where
  show (WrongPiece s) = s
          
toPiece :: String -> Either WrongPieceErr Piece
toPiece pieceStr = 
  case reads pieceStr :: [(Piece, String)] of
    [(s, "")] -> if s == E then emptyNotAllowed else return s
    _ -> Left $ WrongPiece "You can choose either S or O"  
 where 
  emptyNotAllowed =
    Left $ WrongPiece "Player is not allowed to place piece E"
 
readDimensions :: IO Dimensions
readDimensions = do
  boardSize <- toDimensions <$> getLine              
  case boardSize of
    Left err -> print err >> rePrompt >> readDimensions 
    Right dimensions -> return dimensions  
 where
  rePrompt = putStrLn "Again, how big of a board would you like to play on \
                      \ (e.g., 3x3, 4x4, 6x6, etc) ?" 

data WrongDimensionsError = WrongDimensions String
type DimensionsErr = Either WrongDimensionsError

instance Show WrongDimensionsError where
  show (WrongDimensions s) = s
     
toDimensions :: String -> DimensionsErr Dimensions
toDimensions input =
  case reads (filter (/= ' ') input) :: [(Int, String)] of
    [(width, sndPart)] ->
      if (not . null $ sndPart) && (toLower (head sndPart) == 'x')
       then let (_:xs) = sndPart
            in case reads xs :: [(Int, String)] of
                 [(height, _)] -> validateDimensions (width, height)
                 _ -> wrongInput
       else  wrongInput
    _ ->  wrongInput
 where
   wrongInput = Left $
     WrongDimensions "The boards dimensions have to be like\
                     \  so: 3x3, 4x4, 6x6, and so on."


validateDimensions :: Dimensions -> DimensionsErr Dimensions
validateDimensions dim@(width, height)                    
  | notEnough width || notEnough height =
    Left $ WrongDimensions "The board needs to have a size of at least 3x3"
  | width /= height =
    Left $ WrongDimensions "The board width and height needs to be the same "
  | tooMuch width || tooMuch height =
    Left $ WrongDimensions "The maximum allowed board size is 26x26"
  | otherwise = Right dim
 where notEnough x = x < 3
       tooMuch x = x > 26
           
rowNoToRow :: Int -> Board -> Maybe [Piece]
rowNoToRow rowNo board = Map.lookup rowNo $ bPieces board
           
colToRowPos :: Col -> Maybe Int
colToRowPos col = toUpper col `List.elemIndex` ['A' .. 'Z']
           
isWithinBoard :: Board -> PieceCoords -> Bool
isWithinBoard board (rowNo, colNo)
  | rowNo < 0 || colNo < 0 = False
  | otherwise =
      case rowNoToRow rowNo board of
        (Just _) -> let (boardWidth, boardHeight) = bDimensions board 
                    in  colNo < boardWidth && rowNo <= boardHeight
        _ -> False

readPiecePos :: IO PiecePosition
readPiecePos = do       
  piecePos <- filter(/=' ') `liftM` getLine
  if length piecePos < 2 || length piecePos > 3
   then putStrLn "Invalid position provided" >> readPiecePos
   else let (row, [col]) = splitAt (length piecePos - 1) piecePos
        in if isLetter col
            then if all isDigit row
                  then return (read row, col) 
                  else putStrLn "row must be a number" >> readPiecePos
            else putStrLn "column must be a letter" >> readPiecePos
             
piecePosToCoords :: PiecePosition -> PieceCoords
piecePosToCoords (rowNo, col) =
  case colToRowPos col of
    (Just rowPos) -> (rowNo, rowPos)
    __ -> error $ "Column " ++ col:[]  ++ " does not exist"

displayBoard :: Board -> IO ()
displayBoard board = do
  let maxRowNoLen = length . show . fst . bDimensions $ board
  forM_ (Map.toDescList $ bPieces board) $ \(row, pieces) -> do
    putStr $ show row ++ " "                                          
    let rowNoLen = (length . show) row
        latestCoords = bLatestPieceCoords board
        fn = addPaddingToLeftOfBoard rowNoLen maxRowNoLen
    if isPieceOnFirstCol row latestCoords
     then fn (-1)
     else fn 0
    case latestCoords of
      (Just coords) -> do                                                
        let pieceCols = [0..] `zip` pieces
        forM_ pieceCols (\(col, piece) ->
          if (row, col) == coords
           then putStr $ "[" ++ show piece ++ "] |  " 
           else if (row, col + 1) == coords
            then putStr $ show piece ++ "  | " 
            else putStr $ show piece ++ "  |  ")           
      _ -> -- print pieces of each row (initial board state)
          forM_ pieces (\piece ->  putStr $ show piece ++ "  |  ") 
    addRowSeperator 
  alignColumLabelRow maxRowNoLen
  printColumLabels
  addSeperator
  
 where    
   columnDistance = 5
   isPieceOnFirstCol rowNo (Just (x, y)) | rowNo == x && y == 0 = True
   isPieceOnFirstCol _ _ = False
   addPaddingToLeftOfBoard rowNoLen maxRowNoLen adjust =
     replicateM_ (if rowNoLen < maxRowNoLen
                   then maxRowNoLen + 1 + adjust
                   else maxRowNoLen + adjust
                 )
                 (putStr " ")
   alignColumLabelRow maxRowNoLen =
     putChar '\n' >> putStr " " >> replicateM_ maxRowNoLen (putStr "  ")
   printColumLabels = mapM_
     (\col -> putStr $ col:[]  ++ replicate columnDistance ' ')
     (fst $ splitAt (Map.size $ bPieces board) ['A' .. 'Z'])
   addSeperator = replicateM_ 2 (putChar '\n')                     
   addRowSeperator = addSeperator
             
             
displayScore :: Players -> IO ()
displayScore Players{..} = do                     
  let playerList = Map.toList plPlayers
  forM_ playerList (\(_, player) -> putStr (playerName player) >> putChar '\t')
  putChar '\n'
  forM_ playerList (\(_, player) ->
    replicateM_ (length $ playerName player) (putStr "_") >> putChar '\t')
  putChar '\n'
  forM_ playerList (\(_, player) ->
    (putStr . show . playerScore) player >> putChar '\t')
  replicateM_ 2 (putChar  '\n')

outcome :: Players -> String
outcome Players{..} =
  let [fp,sp] = Map.elems plPlayers
  in if fp == sp then "It's a tie"
      else if fp < sp then winner sp
      else winner fp
  
 where winner player = playerName player ++ "is the winner" 

initBoard :: Dimensions -> DimensionsErr Board
initBoard dim@(width, height) =
  case validateDimensions dim of
    (Left e) -> Left e
    _ -> Right $ Board generateBoard (width, height) (width * height) Nothing
  
 where 
   generateBoard =
     let rowNums = [1..width] 
         numOfCols = length rowNums
         row = replicate numOfCols E
     in List.foldl' (\m rowNum -> Map.insert rowNum row m) Map.empty rowNums

foundSeq :: Maybe Piece -> Maybe Piece -> Maybe Piece -> Bool
foundSeq (Just S) (Just O) (Just S) = True
foundSeq _ _ _ = False

numOfCreatedSequences :: Board -> PieceCoords -> Int
numOfCreatedSequences board@Board{..} (rowNo, colNo) =
  numOfHorizontallyCreatedSeqs +
    numOfVerticallyCreatedSeqs +
    numOfDiagnollyCreatedSeqs
 where 
   middleRow = getRow 0 -- the row in which the piece with given coords exists
   upperRow =  getRow 1 
   nextUpperRow = getRow 2
   lowerRow = getRow (-1)
   nextLowerRow = getRow (-2)
   getRow :: Int -> Maybe[Piece]
   getRow offset = rowNoToRow (rowNo + offset) board
   leftCol = toLeft (colNo - 1)
   nextLeftCol = toLeft (colNo - 2)
   toLeft lOffset arr = if lOffset >= 0 then Just(arr `at` lOffset) else Nothing
   rightCol = toRight (colNo + 1) 
   nextRightCol = toRight (colNo + 2)
   toRight rOffset arr = if rOffset < (fst bDimensions)
                          then Just(arr `at` rOffset)
                          else Nothing
 
   -- piece that corresponds to the given coords
   piece = middleRow >>= \cRow -> return (cRow `at` colNo)
  
   numOfHorizontallyCreatedSeqs = length $ List.filter (==True) fns
    where
      fns = [findSeqFromSides, findSeqFromLeft, findSeqFromRight] 
      findSeqFromSides =
        foundSeq (middleRow >>= leftCol) piece (middleRow >>= rightCol) 
      findSeqFromLeft =
        foundSeq (middleRow >>= nextLeftCol) (middleRow >>= leftCol) piece
      findSeqFromRight =
        foundSeq piece (middleRow >>= rightCol) (middleRow >>= nextRightCol)
  
   numOfVerticallyCreatedSeqs = length $ List.filter (==True) fns
    where
      fns = [findSeqInUpperAndLower, findSeqInUpper, findSeqInLower]
      findSeqInUpperAndLower =
        foundSeq (liftM (`at` colNo) upperRow)
                 piece (liftM (`at` colNo) lowerRow)
      findSeqInUpper =
        foundSeq (liftM (`at` colNo) nextUpperRow)
                 (liftM (`at`colNo) upperRow) piece
      findSeqInLower =
        foundSeq piece (liftM (`at`colNo) lowerRow)
                 (liftM (`at`colNo) nextLowerRow)

   numOfDiagnollyCreatedSeqs = length $ List.filter (==True) fns
    where
      fns =
        [ findSeqInUpperLeftAndLowerRight, findSeqInUpperLeft
        , findSeqInLowerRight, findSeqInUpperRightAndLowerLeft
        , findSeqInUpperRight, findSeqInLowerLeft
        ]                                            
      findSeqInUpperLeftAndLowerRight =
        foundSeq upperLeftDiagonalCol piece lowerRightDiagonalCol
      findSeqInUpperLeft  =
        foundSeq nextUpperLeftDiagonalCol upperLeftDiagonalCol piece
      findSeqInLowerRight =
        foundSeq piece lowerRightDiagonalCol nextLowerRightDiagonalCol

      findSeqInUpperRightAndLowerLeft =
        foundSeq upperRightDiagonalCol piece lowerLeftDiagonalCol
      findSeqInUpperRight =
        foundSeq nextUpperRightDiagonalCol upperRightDiagonalCol piece
      findSeqInLowerLeft =
        foundSeq piece lowerLeftDiagonalCol nextLowerLeftDiagonalCol
                                     
      nextUpperRightDiagonalCol = nextUpperRow >>= nextRightCol
      upperRightDiagonalCol = upperRow >>= rightCol
      lowerLeftDiagonalCol = lowerRow >>= leftCol
      nextLowerLeftDiagonalCol = nextLowerRow >>= nextLeftCol
      upperLeftDiagonalCol = upperRow >>= leftCol
      nextUpperLeftDiagonalCol = nextUpperRow >>= nextLeftCol
      lowerRightDiagonalCol = lowerRow >>= rightCol
      nextLowerRightDiagonalCol = nextLowerRow >>= nextRightCol
              
replaceAtIdx :: [a] -> Int -> a -> [a]
replaceAtIdx xs idx newElem =
  let (ys, zs) = splitAt idx xs
      (_:zx) = zs
  in  ys ++ [newElem] ++ zx
