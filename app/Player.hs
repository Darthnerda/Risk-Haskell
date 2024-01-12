module Player where

type PieceCount = Int
type PlayerName = String

data Card = Horse
          | Soldier
          | Cannon
          | Wildcard deriving (Eq)

instance Show Card where
    show Horse = "Bonus Card: Horse"
    show Soldier = "Bonus Card: Soldier"
    show Cannon = "Bonus Card: Cannon"
    show Wildcard = "Bonus Card: Wildcard"

data Player = HumanPlayer { playerName :: PlayerName
                          , unitDeployment :: PieceCount
                          , bonusCards :: [Card] } deriving (Eq)