
module SOS.Player
  ( Player(playerName, playerScore)
  , createPlayer
  , updatePlayerScore
  ) where
        
data Player = P
  { playerName :: String
  , playerScore :: Int
  } deriving Show

instance Eq Player where
  (P _ s1) == (P _ s2) = s1 == s2

instance Ord Player where
  compare (P _ s1) (P _ s2) = compare s1 s2

createPlayer :: String -> Player
createPlayer = flip P 0

updatePlayerScore :: Int -> Player -> Player
updatePlayerScore newPoints player =
  player {playerScore = playerScore player + newPoints}
