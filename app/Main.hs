{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}

import Data.List
import Data.Maybe
import Data.Either
-- import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Error
import Text.Read
import Data.Function
import System.Random

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
                           , pieceCount :: PieceCount } deriving (Eq)

instance Show Territory where
    show t = territoryName t ++ ", " ++ show (pieceCount t)

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

-- Game State Monad
-- newtype Game = Game { getGame :: (Board, [Player], StdGen) }
data Game = Game { getBoard :: Board 
                 , getPlayers :: [Player]
                 , getRandGen :: StdGen }
          | NewGame

-- newtype GameOp a = GameOp { runGame :: ST.StateT Game IO a }
type GameOp a = ST.StateT Game IO a

-- Game Functions

setup :: GameOp Board
setup =
    do  board <- ST.gets getBoard
        let terrsLeft = any (isNothing . owner) $ territories board
        if terrsLeft then do
            terr <- choose
            currentPlayer <- getCurrentPlayer
            replaceTerritory terr { owner = Just $ playerName currentPlayer }
            rotatePlayers
            setup
        else
            ST.gets getBoard
          

play :: GameOp ()
play = do
    board <- ST.gets getBoard
    let playerHoldings = groupBy (\a b -> owner a == owner b) $ territories board
    case playerHoldings of
        [(Territory _ _ _ _ (Just winnerName) _):_] -> ST.liftIO $ putStrLn $ "Congratulations " ++ winnerName ++ ". You won!" -- only matches if exactly one player
        _ -> do ST.liftIO $ putStrLn "Time to play!"
                currentPlayer <- getCurrentPlayer
                deploy $ unitDeployment currentPlayer
                attack
                fortify
                rotatePlayers
                play
                                
        

-- Player Functions

choose :: GameOp Territory
choose = 
    do game <- ST.get
       let board = getBoard game
       currentPlayer <- getCurrentPlayer
       ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", please choose a territory."
       terrChoice <- ST.liftIO getLine
       case getTerr board terrChoice of
            Left e -> do ST.liftIO $ putStrLn $ show e ++ " Please try again."
                         choose
            Right (Territory tn _ _ _ (Just previousOwner) _) -> do ST.liftIO $ putStrLn $ tn ++ " is already owned by " ++ previousOwner ++ ". Please choose a different territory."
                                                                    choose
            Right t -> return t

type DeployCmd = (Territory, PieceCount)

deploy :: PieceCount -> GameOp ()
deploy 0 = return ()
deploy deployRemaining =
    do (terr, dply) <- getDeployCmd deployRemaining
       deployToBoard (terr, dply)
       deploy (deployRemaining - dply)
    
deployToBoard :: DeployCmd -> GameOp ()
deployToBoard (terr, dply) = do game <- ST.get
                                board <- ST.gets getBoard
                                let newTerr = terr { pieceCount = dply + pieceCount terr }
                                    newBoard = fromRight board $ replaceTerr board newTerr
                                ST.put $ game { getBoard = newBoard }

getDeployCmd :: PieceCount -> GameOp DeployCmd
getDeployCmd deployRemaining =
    do game <- ST.get
       board <- ST.gets getBoard
       let plyr@(HumanPlayer pn ud bc) = head . getPlayers $ game
       ST.liftIO $ putStrLn $ pn ++ ", you have " ++ show deployRemaining ++ " deployment remaining. Type a territory name and the number of units you would like to deploy there."
       userInput <- ST.liftIO getLine
       case parseDeployCmd board plyr deployRemaining userInput of
            Left err -> (do ST.liftIO $ print err
                            getDeployCmd deployRemaining)
            Right (terr, dply) -> return (terr, dply)

parseDeployCmd :: Board -> Player -> PieceCount -> String -> Either BoardError DeployCmd
parseDeployCmd board plyr deployRemaining cmd
    | [terrName, deployCount] <- split = 
        do dply <- case readMaybe deployCount :: Maybe PieceCount of
                        Nothing -> Left $ TypeError "Invalid deploy argument"
                        Just dp -> Right dp
           if dply < 0 then 
                Left $ ValueError "You can't deploy a negative amount."
           else Right ()
           if dply > deployRemaining then
                Left $ ValueError "You can't deploy more than your remaining deployment."
            else Right ()
           terr <- getTerr board terrName
           if playerName plyr == fromJust (owner terr)
              then return ()
              else Left $ ValueError "That's not your territory! Pick another you fool!"
           return (terr, dply)
    | [_, _, xs] <- split = Left $ ArityError $ "Too many arguments. What the heck are " ++ xs ++ "?"
    | [_] <- split = Left $ ArityError "Too few arguments. You need two here."
    | [] <- split = Left $ ArityError "No arguments at all? What are you crazy? You'll blow up the universe. Try again, bucko."
    where split = words cmd


parseDeployCmd2 :: (MonadError BoardError m, ST.MonadState Game m) => String -> PieceCount -> m DeployCmd
parseDeployCmd2 cmd deployRemaining = 
    do  let split = words cmd
        when (length split /= 2) $ throwError $ ArityError "Needs exactly 2 arguments fool."
        terr <- getTerrST $ head split
        player <- ST.gets (head . getPlayers)
        when (playerName player /= fromMaybe "" (owner terr)) $ throwError $ ValueError "That's not your territory! Pick another you fool!"
        pcs <- readPieceCount $ split !! 1
        when (pcs < 0) $ throwError $ ValueError "You can't deploy less than 0."
        when (pcs > deployRemaining) $ throwError $ ValueError "You can't deploy more than youre remaining deployment."
        pure (terr, pcs)

type AttackCmd = (Territory, Territory, PieceCount)

parseAttackCmd :: (MonadError BoardError m, ST.MonadState Game m) => String -> m AttackCmd
parseAttackCmd cmd = 
    do  let split = words cmd
        when (length split /= 3) $ throwError $ ArityError "Needs exactly 3 arguments fool."
        sTerr <- getTerrST $ head split
        dTerr <- getTerrST $ split !! 1
        ownS <- isOwner sTerr
        ownD <- isOwner dTerr
        unless ownS $ throwError $ ValueError "That's not your territory! Pick another you fool!"
        when ownD $ throwError $ ValueError "You can't attack your own territory. Wait until the fortify step to move units."
        pcs <- readPieceCount $ split !! 2
        when (pcs > pieceCount sTerr - 1) $ throwError $ ValueError "You can't attack with more pieces than you have on the territory minus one."
        when (pcs < 1) $ throwError $ ValueError "You must attack with at least one piece."
        pure (sTerr, dTerr, pcs)




attack :: GameOp ()
attack =
    do  currentPlayer <- getCurrentPlayer
        playerTerrs <- getPlayerTerrs currentPlayer
        ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", your territories include: "
        ST.liftIO $ sequence $ map (putStr . show) playerTerrs
        ST.liftIO $ putStrLn "During attack phase you have access to the following commands:"
        ST.liftIO $ putStrLn "board -- shows you the current state of the whole board"
        ST.liftIO $ putStrLn "player [PLAYERNAME] -- shows you the stats of the given player"
        ST.liftIO $ putStrLn "attack [SOURCE] [TARGET] [PIECES] -- Attack from source territory to target territoy with that many pieces."
        return ()

fortify :: GameOp ()
fortify = 
    do ST.liftIO $ putStrLn "We fortiy!"
       return ()

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

getTerrST :: (MonadError BoardError m, ST.MonadState Game m) => TerritoryName -> m Territory
getTerrST terrName = do terrs <- ST.gets (territories . getBoard)
                        case find ((terrName ==) . territoryName) terrs of
                            Just terr -> pure terr
                            Nothing -> throwError $ NoSuchTerritory terrName

readPieceCount :: (MonadError BoardError m, ST.MonadState Game m) => String -> m PieceCount
readPieceCount pc = case readMaybe pc of
                        (Just pcp) -> pure pcp
                        Nothing -> throwError $ ValueError "Malformed piece count argument."

terrsInCont :: Board -> ContinentName -> Either BoardError [Territory]
terrsInCont board contName = if contName `elem` contNames
    then Right $ filter ((contName ==) . continent) $ territories board
    else Left $ NoSuchContinent contName
    where contNames = map continentName $ continents board

rotatePlayers :: GameOp ()
rotatePlayers = do game <- ST.get
                   let (firstPlayer:restPlayers) = getPlayers game
                   ST.put $ game {getPlayers = restPlayers ++ [firstPlayer]}

getCurrentPlayer :: GameOp Player
getCurrentPlayer = ST.gets (head . getPlayers)

getCurrentPlayerST :: (MonadError BoardError m, ST.MonadState Game m) => m Player
getCurrentPlayerST = ST.gets (head . getPlayers)

isOwner :: (MonadError BoardError m, ST.MonadState Game m) => Territory -> m Bool
isOwner terr = do   player <- getCurrentPlayerST
                    pure $ playerName player == fromMaybe "" (owner terr)

updateBoard :: Board -> GameOp ()
updateBoard newBoard = do game <- ST.get
                          ST.put $ game { getBoard = newBoard }

getPlayerTerrs :: Player -> GameOp [Territory]
getPlayerTerrs plyr = do terrs <- ST.gets (territories . getBoard)
                         return $ filter ( (== playerName plyr) . fromJust . owner ) terrs

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

eitherGuard :: a -> (a ->  Bool) -> e -> Either e a
eitherGuard x p e = if p x then Right x else Left e

maybeRight :: Either e a -> Maybe a
maybeRight (Right x) = Just x
maybeRight (Left _) = Nothing

replaceOn :: (Functor f) => (a -> a -> Bool) -> a -> f a -> f a
replaceOn p v = fmap (\x -> if p x v then v else x)

replaceTerritory :: Territory -> GameOp ()
replaceTerritory terr = do game <- ST.get
                           let  board = getBoard game
                                terrs = territories board
                                newTerrs = replaceOn ((==) `on` territoryName) terr terrs
                           ST.put $ game { getBoard =  board { territories = newTerrs }}
                           return ()

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act init =
   when (test init) $ act init >>= whileM test act

initialBoard = fromRight (Board [] []) $ bulkAdd Nothing
                                          [("Asia", 7), ("North America", 5), ("South America", 2)]
                                          [ ("Kamchatka", ["Alaska"], "Asia", 1)
                                          , ("Alaska", ["Kamchatka", "Alberta"], "North America", 1)
                                          , ("Alberta", ["Alaska"], "North America", 1)
                                          , ("Peru", ["Brazil"], "South America", 1)
                                          , ("Brazil", ["Peru"], "South America", 1)]

players = [ HumanPlayer "Josh" 3 []
          , HumanPlayer "Nathan" 3 [] ]

-- main = runGame $ ( do { setupBoard <- setup; play; ST.liftIO $ print setupBoard } ) (Game )

main :: IO ()
main = do gen <- getStdGen
          endBoard <- ST.evalStateT ( do _ <- setup
                                         play
                                         ST.gets getBoard )
                        $ Game initialBoard players gen
          print endBoard
    
    -- cNames <- Right $ map continentName $ continents board
    -- scores <- mapM (bonusPerTerr board) cNames
    -- Right $ zip cNames scores