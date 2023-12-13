import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Text.Read
import Data.Function

-- Type Aliases

type TerritoryName = String
type ContinentName = String
type Bonus = Int
type PlayerName = String
type PieceCount = Int

-- ADTs

data Territory = Territory { territoryName :: TerritoryName
                           , connections :: [TerritoryName]
                           , continent :: ContinentName
                           , initPieces :: PieceCount
                           , owner :: Maybe PlayerName
                           , pieceCount :: PieceCount } deriving (Show, Eq)

data Continent = Continent { continentName :: ContinentName
                           , bonus :: Bonus } deriving Show

data Board = Board { territories :: [Territory]
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

data Card = Horse
          | Soldier
          | Cannon
          | Wildcard

data Player = HumanPlayer { playerName :: PlayerName
                          , unitDeployment :: PieceCount
                          , bonusCards :: [Card] }

-- Game Functions

setup :: Board -> [Player] -> IO (Board, [Player])
setup board plyrs@(firstPlayer:restPlayers)
    | terrsLeft = do terr <- choose board firstPlayer
                     let updatedTerr = terr {owner = Just $ playerName firstPlayer}
                         newBoard = fromRight board $ replaceTerr board updatedTerr
                     setup newBoard $ restPlayers ++ [firstPlayer]
    | otherwise = return (board, plyrs)
    where terrsLeft = any (isNothing . owner) $ territories board

play :: Board -> [Player] -> IO (Board, [Player])
play board plyrs@(firstPlayer:restPlayers)
    | [(Territory _ _ _ _ (Just winnerName) _):xs] <- playerHoldings =
        do putStrLn $ "Congratulations " ++ winnerName ++ ". You won!"
           return (board, plyrs)
    | otherwise = do putStrLn "Uhhhhhhhhh"
                     (dBoard, dPlayer) <- deploy board firstPlayer (unitDeployment firstPlayer)
                     (aBoard, aPlayer, aRestPlayers) <- attack dBoard dPlayer restPlayers
                     (fBoard, fPlayer) <- fortify aBoard aPlayer
                     play fBoard $ aRestPlayers ++ [fPlayer]
    where playerHoldings = groupBy (\a b -> owner a == owner b) $ territories board

-- Player Functions

choose :: Board -> Player -> IO Territory
choose board plyr = 
    do putStrLn $ playerName plyr ++ ", please choose a territory."
       terrChoice <- getLine
       case getTerr board terrChoice of
            Left e -> do putStrLn $ show e ++ " Please try again."
                         choose board plyr
            Right (Territory tn _ _ _ (Just previousOwner) _) -> do putStrLn $ tn ++ " is already owned by " ++ previousOwner ++ ". Please choose a different territory."
                                                                    choose board plyr
            Right t -> return t

type DeployCmd = (Territory, PieceCount)

deploy :: Board -> Player -> PieceCount -> IO (Board, Player)
deploy board plyr 0 = return (board, plyr)
deploy board plyr deployRemaining =
    do (terr, dply) <- getDeployCmd board plyr deployRemaining
       let newBoard = deployToBoard board (terr, dply)
       deploy newBoard plyr (deployRemaining - dply)
    
deployToBoard :: Board -> DeployCmd -> Board
deployToBoard board (terr, dply) = let newTerr = terr { pieceCount = dply + pieceCount terr }
                                   in fromRight board $ replaceTerr board newTerr



getDeployCmd :: Board -> Player -> PieceCount -> IO DeployCmd
getDeployCmd board plyr@(HumanPlayer pn ud bc) deployRemaining =
    do putStrLn $ pn ++ ", you have " ++ show deployRemaining ++ " deployment remaining. Type a territory name and the number of units you would like to deploy there."
       userInput <- getLine
       case parseDeployCmd board plyr deployRemaining userInput of
            Left err -> (do print err
                            getDeployCmd board plyr deployRemaining)
            Right (terr, dply) -> return (terr, dply)

parseDeployCmd :: Board -> Player -> PieceCount -> String -> Either BoardError DeployCmd
parseDeployCmd board plyr deployRemaining cmd
    | [terrName, deployCount] <- split = 
        do dply <- case readMaybe deployCount :: Maybe PieceCount of
                        Nothing -> Left $ TypeError "Invalid deploy argument"
                        Just dp -> Right dp
           terr <- getTerr board terrName
           if playerName plyr == fromJust (owner terr)
              then return ()
              else Left $ ValueError "That's not your territory! Pick another you fool!"
           return (terr, dply)
    | [_, _, xs] <- split = Left $ ArityError $ "Too many arguments. What the heck are " ++ xs ++ "?"
    | [_] <- split = Left $ ArityError "Too few arguments. You need two here."
    | [] <- split = Left $ ArityError "No arguments at all? What are you crazy? You'll blow up the universe. Try again, bucko."
    where split = words cmd


attack :: Board -> Player -> [Player] -> IO (Board, Player, [Player])
attack b p rp =
    do putStrLn "We attack!"
       return (b, p, rp)

fortify :: Board -> Player -> IO (Board, Player)
fortify b p = 
    do putStrLn "We fortiy!"
       return (b, p)

-- Board Build Functions

insertTerritory :: Board -> TerritoryName -> ContinentName -> PieceCount -> Either BoardError Board
insertTerritory board@(Board terrs conts) newTName contName initPieces = do
        getCont board contName -- Just to ensure that the continent exists
        ( let newTerritory = Territory newTName [] contName initPieces Nothing initPieces
          in Right $ Board (newTerritory : terrs) conts )

insertTerritories :: Board -> [(TerritoryName, ContinentName, PieceCount)] -> Either BoardError Board
insertTerritories = foldM (\b (tn, cn, ip) -> insertTerritory b tn cn ip)

addConnection :: Board -> TerritoryName -> TerritoryName -> Either BoardError Board
addConnection board baseTName otherTName =
    do (Territory _ baseConns baseCont baseIp baseOwn basePc) <- getTerr board baseTName
       (Territory _ otherConns otherCont otherIp otherOwn otherPc) <- getTerr board otherTName
       (let newBase = Territory baseTName (nub $ otherTName : baseConns) baseCont baseIp baseOwn basePc
            newOther = Territory otherTName (nub $ baseTName : otherConns) otherCont otherIp otherOwn otherPc
        in foldM replaceTerr board [newBase, newOther])

replaceTerr :: Board -> Territory -> Either BoardError Board
replaceTerr (Board terrs conts) newT = 
    fromMaybe (Left $ NoSuchTerritory tName)
              $ do idx <- elemIndex tName $ map territoryName terrs
                   newTerrs <- replaceElemAt terrs idx newT
                   Just $ Right (Board newTerrs conts)
    where tName = territoryName newT

addConnections :: Board -> [(TerritoryName, TerritoryName)] -> Either BoardError Board
addConnections = foldM (\b (bn, on) -> addConnection b bn on)

bulkAdd :: Maybe Board -> [(ContinentName, Bonus)] -> [(TerritoryName, [TerritoryName], ContinentName, PieceCount)] -> Either BoardError Board
bulkAdd maybeBoard contInfos terrInfos = 
    do withTerrs <- insertTerritories withConts [(tName, cName, initPieces) | (tName, _, cName, initPieces) <- terrInfos]
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
getTerr board terrName = case find ((terrName ==) . territoryName) $ territories board of
    Just terr -> Right terr
    Nothing -> Left $ NoSuchTerritory terrName

terrsInCont :: Board -> ContinentName -> Either BoardError [Territory]
terrsInCont board contName = if contName `elem` contNames
    then Right $ filter ((contName ==) . continent) $ territories board
    else Left $ NoSuchContinent contName
    where contNames = map continentName $ continents board

-- Stats Functions

bonusPerTerr :: Board -> ContinentName -> Either BoardError Float
bonusPerTerr board contName = do
    terrs <- terrsInCont board contName
    cont <- getCont board contName
    return $ (fromIntegral $ bonus cont) / (fromIntegral $ length terrs)

-- Utility Functions

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

replaceElemAt :: (Eq a) => [a] -> Int -> a -> Maybe [a]
replaceElemAt lst idx newVal = if idx > length lst - 1 
                               then Nothing
                               else let (x, _:ys) = splitAt idx lst 
                                    in Just (x ++ newVal : ys)

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act init =
   when (test init) $ act init >>= whileM test act

board = fromRight (Board [] []) $ bulkAdd Nothing
                                          [("Asia", 7), ("North America", 5), ("South America", 2)]
                                          [ ("Kamchatka", ["Alaska"], "Asia", 1)
                                          , ("Alaska", ["Kamchatka", "Alberta"], "North America", 1)
                                          , ("Alberta", ["Alaska"], "North America", 1)
                                          , ("Peru", ["Brazil"], "South America", 1)
                                          , ("Brazil", ["Peru"], "South America", 1)]

players = [ HumanPlayer "Josh" 3 []
          , HumanPlayer "Nathan" 3 [] ]

main = do (setupBoard, setupPlayers) <- setup board players
          (playedBoard, playedPlayers) <- play setupBoard setupPlayers
          print setupBoard
    
    -- cNames <- Right $ map continentName $ continents board
    -- scores <- mapM (bonusPerTerr board) cNames
    -- Right $ zip cNames scores