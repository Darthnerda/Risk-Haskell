module Board where

import Player
import Control.Lens
import Utility
import Data.Maybe
import Control.Monad
import Data.List

type Bonus = Int
type TerritoryName = String
type ContinentName = String


data Territory = Territory { territoryName :: TerritoryName
                           , connections :: [TerritoryName]
                           , continent :: ContinentName
                           , initPieces :: PieceCount
                           , owner :: Maybe PlayerName
                           , pieceCount :: PieceCount } deriving (Eq)

instance Show Territory where
    show t = territoryName t ++ ", " ++ show (pieceCount t)

data Continent = Continent { continentName :: ContinentName
                           , bonus :: Bonus } deriving Show

data Board = Board { _territories :: [Territory]
                   , continents :: [Continent]} deriving Show

data BoardError = NoSuchContinent ContinentName
                | NoSuchTerritory TerritoryName
                | ArityError String
                | TypeError String
                | ValueError String

instance Show BoardError where
    show (NoSuchContinent cn) = "No such continent: " ++ cn ++ "."
    show (NoSuchTerritory tn) = "No such territory: " ++ tn ++ "."
    show (ArityError e) = "Arity Error: " ++ e ++ "."
    show (TypeError e) = "Type Error: " ++ e ++ "."
    show (ValueError e) = "Type Error: " ++ e ++ "."

territories :: Lens' Board [Territory]
territories = lens _territories (\p newTerritories -> p { _territories = newTerritories })

insertTerritory :: Board -> TerritoryName -> ContinentName -> PieceCount -> Either BoardError Board
insertTerritory brd@(Board terrs conts) newTName contName _initPieces = do
        _ <- getCont brd contName -- Just to ensure that the continent exists
        ( let newTerritory = Territory newTName [] contName _initPieces Nothing _initPieces
          in Right $ Board (newTerritory : terrs) conts )

insertTerritories :: Board -> [(TerritoryName, ContinentName, PieceCount)] -> Either BoardError Board
insertTerritories = foldM (\b (tn, cn, ip) -> insertTerritory b tn cn ip)

addConnection :: Board -> TerritoryName -> TerritoryName -> Either BoardError Board
addConnection brd baseTName otherTName =
    do (Territory _ baseConns baseCont baseIp baseOwn basePc) <- getTerr brd baseTName
       (Territory _ otherConns otherCont otherIp otherOwn otherPc) <- getTerr brd otherTName
       (let newBase = Territory baseTName (nub $ otherTName : baseConns) baseCont baseIp baseOwn basePc
            newOther = Territory otherTName (nub $ baseTName : otherConns) otherCont otherIp otherOwn otherPc
        in foldM replaceTerr brd [newBase, newOther])

replaceTerr :: Board -> Territory -> Either BoardError Board
replaceTerr (Board terrs conts) newT = 
    fromMaybe (Left $ NoSuchTerritory tName)
              $ do idx <- elemIndex tName $ map territoryName terrs
                   newTerrs <- replaceElemAt terrs idx newT
                   Just $ Right (Board newTerrs conts)
    where tName = territoryName newT

addConnections :: Board -> [(TerritoryName, TerritoryName)] -> Either BoardError Board
addConnections = foldM (\b (bn, onn) -> addConnection b bn onn)

bulkAdd :: Maybe Board -> [(ContinentName, Bonus)] -> [(TerritoryName, [TerritoryName], ContinentName, PieceCount)] -> Either BoardError Board
bulkAdd maybeBoard contInfos terrInfos = 
    do withTerrs <- insertTerritories withConts [(tName, cName, _initPieces) | (tName, _, cName, _initPieces) <- terrInfos]
       connInfos <- Right $ flatten [[(tName, otherName) | otherName <- otherNames] | (tName, otherNames, _, _) <- terrInfos]
       addConnections withTerrs connInfos
    where (Board baseTerrs baseConts) = fromMaybe (Board [] []) maybeBoard
          withConts = let newConts = map (uncurry Continent) contInfos
                      in Board baseTerrs (newConts ++ baseConts)

-- Board Utility Functions

getCont :: Board -> ContinentName -> Either BoardError Continent
getCont board contName = case find ((contName ==) . continentName) $ continents board of
    Just cont -> Right cont
    Nothing -> Left $ NoSuchContinent contName
    
getTerr :: Board -> TerritoryName -> Either BoardError Territory
getTerr board terrName = case board^.territories.to (find ((terrName ==) . territoryName)) of
    Just terr -> Right terr
    Nothing -> Left $ NoSuchTerritory terrName

terrsInCont :: Board -> ContinentName -> Either BoardError [Territory]
terrsInCont board contName = if contName `elem` contNames
    then Right $ board^.territories.to (filter ((contName ==) . continent))
    else Left $ NoSuchContinent contName
    where contNames = map continentName $ continents board