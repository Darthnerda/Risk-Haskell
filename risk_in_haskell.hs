import Data.List
import Data.Maybe
import Data.Either
import Control.Monad

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
    | terrsLeft = do (Territory a b c d _ f) <- choose board firstPlayer
                     let updatedTerr = (Territory a b c d (Just $ playerName firstPlayer) f)
                         newBoard = fromRight board $ replaceTerr board updatedTerr
                     setup newBoard $ restPlayers ++ [firstPlayer]
    | otherwise = return (board, plyrs)
    where terrsLeft = any isNothing $ map owner $ territories board

play :: Board -> [Player] -> IO (Board, [Player])
play board plyrs@(firstPlayer:restPlayers)
    | [((Territory _ _ _ _ (Just winnerName) _):xs)] <- playerHoldings =
        do putStrLn $ "Congratulations " ++ winnerName ++ ". You won!"
           return (board, plyrs)
    | otherwise = do putStrLn "Uhhhhhhhhh"
                     (dBoard, dPlayer) <- deploy board firstPlayer
                     (aBoard, aPlayer) <- attack dBoard dPlayer restPlayers
                     (fBoard, fPlayer) <- fortify aBoard aPlayer
                     play fBoard restPlayers ++ [fPlayer]
    where playerHoldings = groupBy (\a b -> owner a == owner b) $ territories board

-- Player Functions

choose :: Board -> Player -> IO Territory
choose board plyr@(HumanPlayer pn _ _) = 
    do putStrLn $ pn ++ ", please choose a territory."
       terrChoice <- getLine
       case getTerr board terrChoice of
            Left e -> do putStrLn $ show e ++ " Please try again."
                         choose board plyr
            Right (Territory tn _ _ _ (Just previousOwner) _) -> do putStrLn $ tn ++ " is already owned by " ++ previousOwner ++ ". Please choose a different territory."
                                                                    choose board plyr
            Right t -> return t

deploy :: Board -> Player -> IO (Board, Player)
deploy b plyr@(HumanPlayer pn ud bc) = 
    do deploys <- sequence $ whileM (\(terr, dply) -> dply > 0) $ iterateM prompt $ return ()
       forM deploys (\)
    where prompt deployRemaining = 
          do putStrLn $ pn ++ ", you have " ++ deployRemaining ++ " deployment remaining. Type a territory name and the number of units you would like to deploy there."
             userInput <- getLine
             case parseDeployCmd userInput of
                  Left err = do putStrLn $ show err
                                prompt deployRemaining
                  Right (terr, dply) = return (terr, dply)
          deployToBoard ((Territory a b c d e f), dply) -> let newTerr = (Territory a b c d e (f + dply))
                                                                 newBoard = fromRight b $ replaceTerr b newTerr
                                                                 return (newBoard, plyr)

parseDeployCmd :: String -> Either BoardError (Territory, PieceCount)
parseDeployCmd cmd
    | (terrName, deployCount : []) <- split = do dply -> case readMaybe deployCount :: PieceCount of
                                                         Nothing -> Left $ TypeError "Invalid deploy argument"
                                                         Just dp -> Right dp
                                                 terr -> getTerr 
    | (_, _ : xs) <- split = Left $ ArityError $ "Too many arguments. What the heck are " ++ $ unwords xs ++ "?"
    | (_ : []) <- split = Left $ ArityError $ "Too few arguments. You need two here."
    | [] <- split = Left $ ArityError $ "No arguments at all? What are you crazy? You'll blow up the universe. Try again, bucko."
    where split = words cmd


-- attack :: Board -> Player -> IO (Either BoardError Board, Player)

-- fortify :: Board -> Player -> IO (Either Board, Player)

-- Board Build Functions

insertTerritory :: Board -> TerritoryName -> ContinentName -> PieceCount -> Either BoardError Board
insertTerritory board@(Board terrs conts) newTName contName initPieces = do
        getCont board contName -- Just to ensure that the continent exists
        ( let newTerritory = (Territory newTName [] contName initPieces Nothing initPieces)
          in Right $ Board (newTerritory : terrs) conts )

insertTerritories :: Board -> [(TerritoryName, ContinentName, PieceCount)] -> Either BoardError Board
insertTerritories board terrInfos = foldM (\b (tn, cn, ip) -> insertTerritory b tn cn ip) board terrInfos

addConnection :: Board -> TerritoryName -> TerritoryName -> Either BoardError Board
addConnection board baseTName otherTName =
    do (Territory _ baseConns baseCont baseIp baseOwn basePc) <- getTerr board baseTName
       (Territory _ otherConns otherCont otherIp otherOwn otherPc) <- getTerr board otherTName
       (let newBase = (Territory baseTName (nub $ otherTName : baseConns) baseCont baseIp baseOwn basePc)
            newOther = (Territory otherTName (nub $ baseTName : otherConns) otherCont otherIp otherOwn otherPc)
        in foldM replaceTerr board [newBase, newOther])

replaceTerr :: Board -> Territory -> Either BoardError Board
replaceTerr (Board terrs conts) newT = 
    fromMaybe (Left $ NoSuchTerritory tName)
              $ do idx <- findIndex (== tName) $ map territoryName terrs
                   newTerrs <- replaceElemAt terrs idx newT
                   Just $ Right (Board newTerrs conts)
    where tName = territoryName newT

addConnections :: Board -> [(TerritoryName, TerritoryName)] -> Either BoardError Board
addConnections board connInfos = foldM (\b (bn, on) -> addConnection b bn on) board connInfos

bulkAdd :: Maybe Board -> [(ContinentName, Bonus)] -> [(TerritoryName, [TerritoryName], ContinentName, PieceCount)] -> Either BoardError Board
bulkAdd maybeBoard contInfos terrInfos = 
    do withTerrs <- insertTerritories withConts [(tName, cName, initPieces) | (tName, _, cName, initPieces) <- terrInfos]
       connInfos <- Right $ flatten [[(tName, otherName) | otherName <- otherNames] | (tName, otherNames, _, _) <- terrInfos]
       withConns <- addConnections withTerrs connInfos
       return withConns
    where (Board baseTerrs baseConts) = fromMaybe (Board [] []) maybeBoard
          withConts = let newConts = map (\(cn, b) -> (Continent cn b)) contInfos
                      in (Board baseTerrs (newConts ++ baseConts))

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
terrsInCont board contName = if (contName `elem` contNames)
    then Right $ filter ((contName ==) . continent) $ territories board
    else Left $ NoSuchContinent contName
    where contNames = (map continentName $ continents board)

-- Stats Functions

bonusPerTerr :: Board -> ContinentName -> Either BoardError Float
bonusPerTerr board contName = do
    terrs <- terrsInCont board contName
    cont <- getCont board contName
    return $ (fromIntegral $ bonus cont) / (fromIntegral $ length terrs)

-- Utility Functions

flatten :: [[a]] -> [a]
flatten lst = foldr1 (++) lst

replaceElemAt :: (Eq a) => [a] -> Int -> a -> Maybe [a]
replaceElemAt lst idx newVal = if idx > (length lst) - 1 
                               then Nothing
                               else let (x, _:ys) = splitAt idx lst 
                                    in Just (x ++ newVal : ys)

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act init =
   when (test init) $ (act init) >>= whileM test act

board = fromRight (Board [] []) $ bulkAdd Nothing
                                          [("Asia", 7), ("North America", 5), ("South America", 2)]
                                          [ ("Kamchatka", ["Alaska"], "Asia", 1)
                                          , ("Alaska", ["Kamchatka", "Alberta"], "North America", 1)
                                          , ("Alberta", ["Alaska"], "North America", 1)
                                          , ("Peru", ["Brazil"], "South America", 1)
                                          , ("Brazil", ["Peru"], "South America", 1)]

players = [(HumanPlayer "Josh" 3 []), (HumanPlayer "Nathan" 3 [])]

main = do (setupBoard, setupPlayers) <- setup board players
          print setupBoard
    
    -- cNames <- Right $ map continentName $ continents board
    -- scores <- mapM (bonusPerTerr board) cNames
    -- Right $ zip cNames scores