module Player where

type PieceCount = Int
type PlayerName = String

data Card = Horse
          | Soldier
          | Cannon
          | Wildcard deriving (Eq)

data Player = HumanPlayer { playerName :: PlayerName
                          , unitDeployment :: PieceCount
                          , bonusCards :: [Card] } deriving (Eq)