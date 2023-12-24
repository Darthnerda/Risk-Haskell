{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import qualified Control.Monad.State as ST
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Text.Read
import Data.Function
import System.Random
import Data.Ord

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

-- newtype GameOp a = GameOp { runGame :: ST.StateT Game IO a }
type GameOp a = ST.StateT Game IO a

-- Game Functions

-- Setup

choose :: (ST.MonadState Game m, MonadIO m) => m Territory
choose =
    do game <- ST.get
       let board = getBoard game
       currentPlayer <- getCurrentPlayerST
       ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", please choose a territory."
       terrChoice <- ST.liftIO getLine
       case getTerr board terrChoice of
            Left e -> do ST.liftIO $ putStrLn $ show e ++ " Please try again."
                         choose
            Right (Territory tn _ _ _ (Just previousOwner) _) -> do ST.liftIO $ putStrLn $ tn ++ " is already owned by " ++ previousOwner ++ ". Please choose a different territory."
                                                                    choose
            Right t -> return t

setup :: (ST.MonadState Game m, MonadIO m) => m Board
setup =
    do  board <- ST.gets getBoard
        let terrsLeft = any (isNothing . owner) $ territories board
        if terrsLeft then do
            terr <- choose
            currentPlayer <- getCurrentPlayerST
            replaceTerrST terr { owner = Just $ playerName currentPlayer }
            rotatePlayers2
            setup
        else
            ST.gets getBoard

-- Play

play :: (ST.MonadState Game m, MonadIO m) => m ()
play = do
    board <- ST.gets getBoard
    let playerHoldings = groupBy (\a b -> owner a == owner b) $ territories board
    case playerHoldings of
        [(Territory _ _ _ _ (Just winnerName) _):_] -> ST.liftIO $ putStrLn $ "Congratulations " ++ winnerName ++ ". You won!" -- only matches if exactly one player
        _ -> do ST.liftIO $ putStrLn "Time to play!"
                currentPlayer <- getCurrentPlayerST
                deploy $ unitDeployment currentPlayer
                attack
                fortify
                rotatePlayers2
                play
        

-- Player Functions

-- Deploy

type DeployCmd = (Territory, PieceCount)

deploy :: (ST.MonadState Game m, MonadIO m) => PieceCount -> m ()
deploy 0 = return ()
deploy deployRemaining =
    do (terr, dply) <- getDeployCmd deployRemaining
       deployToBoard (terr, dply)
       deploy (deployRemaining - dply)
    
deployToBoard :: (ST.MonadState Game m) => DeployCmd -> m ()
deployToBoard (terr, dply) = replaceTerrST $ terr { pieceCount = dply + pieceCount terr }

getDeployCmd :: (ST.MonadState Game m, MonadIO m) => PieceCount -> m DeployCmd
getDeployCmd deployRemaining =
    do currentPlayer <- getCurrentPlayerST
       ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", you have " ++ show deployRemaining ++ " deployment remaining. Type a territory name and the number of units you would like to deploy there."
       userInput <- ST.liftIO getLine
       result <- runExceptT $ parseDeployCmd userInput deployRemaining
       case result of
            Left err -> (do ST.liftIO $ print err
                            getDeployCmd deployRemaining)
            Right (terr, dply) -> return (terr, dply)

parseDeployCmd :: (MonadError BoardError m, ST.MonadState Game m) => String -> PieceCount -> m DeployCmd
parseDeployCmd cmd deployRemaining = 
    do  let split = words cmd
        when (length split /= 2) $ throwError $ ArityError "Needs exactly 2 arguments fool."
        terr <- getTerrST $ head split
        player <- ST.gets (head . getPlayers)
        when (playerName player /= fromMaybe "" (owner terr)) $ throwError $ ValueError "That's not your territory! Pick another you fool!"
        pcs <- readPieceCount $ split !! 1
        when (pcs < 0) $ throwError $ ValueError "You can't deploy less than 0."
        when (pcs > deployRemaining) $ throwError $ ValueError "You can't deploy more than youre remaining deployment."
        pure (terr, pcs)

-- Attack

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

getAttackCmd :: (ST.MonadState Game m, MonadIO m) => MaybeT m AttackCmd
getAttackCmd =
    do  ST.liftIO $ putStrLn "Type your command now. Choose wisely."
        cmd <- ST.liftIO getLine
        guard (cmd /= "done")
        do  result <- runExceptT $ parseAttackCmd cmd
            case    result of
                    Left e -> do    ST.liftIO $ putStrLn $ show e ++ " Please try again."
                                    getAttackCmd
                    Right atkCmd -> return atkCmd


newtype BattleStats = BattleStats { getBattleStats :: (([Word], [Word]), (PieceCount, PieceCount), (PieceCount, PieceCount), String) }
instance Show BattleStats where
    show (BattleStats ((aD, dD), (aLoss, dLoss), (aNew, dNew), msg))
        =  "Attacker rolled: " ++ foldr1 (++) (intersperse ", " (map show aD)) ++ "\n"
        ++ "Defender rolled: " ++ foldr1 (++) (intersperse ", " (map show dD)) ++ "\n"
        ++ "Attacker lost " ++ show aLoss ++ " pieces.\n"
        ++ "Defender lost " ++ show dLoss ++ " pieces.\n"
        ++ "Attacker now has " ++ show aNew ++ " pieces.\n"
        ++ "Defender now has " ++ show dNew ++ " pieces.\n"
        ++ msg

battle :: (ST.MonadState Game m) => AttackCmd -> m BattleStats
battle (sTerr, dTerr, apc) =
    do  aDice <- fmap sortDesc $ withRandGen rolls $ clamp (1, 3) apc
        dDice <- fmap sortDesc $ withRandGen rolls $ clamp (1, 3) dpc
        let outcomes = zipWith (>) aDice dDice
            (newApc, newDpc) = foldr (\aWon (a,b) -> if aWon then (a, b-1) else (a-1,b)) (apc, dpc) outcomes
            attackerLosses = apc - newApc
            defenderLosses = dpc - newDpc
        msg <-  if newDpc <= 0 then -- Attacker takes over ownership of defender's territory, and attacker's remaining attack pieces move there
                    do  replaceTerrST dTerr { pieceCount = newApc
                                            , owner = owner sTerr }
                        replaceTerrST sTerr { pieceCount = pieceCount sTerr - apc }
                        return $ "Attacker took over " ++ territoryName dTerr ++ " and moved " ++ show newApc ++ " pieces in."
                
                else -- Attacker and defender take losses
                    do  replaceTerrST dTerr { pieceCount = newDpc }
                        replaceTerrST sTerr { pieceCount = pieceCount sTerr - attackerLosses }
                        return $ "Defender successfully defended" ++ territoryName dTerr ++ "."
        return $ BattleStats ((aDice, dDice), (attackerLosses, defenderLosses), (pieceCount sTerr - attackerLosses, newDpc), msg)
    where dpc = pieceCount dTerr


attack :: (ST.MonadState Game m, MonadIO m) => m ()
attack =
    do  currentPlayer <- getCurrentPlayerST
        playerTerrs <- getPlayerTerrs currentPlayer
        ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", your territories include: "
        _ <- ST.liftIO $ sequence $ map print playerTerrs
        ST.liftIO $ putStrLn "You have two commands at your disposal:"
        ST.liftIO $ putStrLn "attack [SOURCE] [TARGET] [PIECES] -- Attack from source territory to target territoy with that many pieces."
        ST.liftIO $ putStrLn "done -- Ends your attack phase."
        maybeAtkCmd <- runMaybeT getAttackCmd 
        case maybeAtkCmd of
            Nothing -> return ()
            Just atkCmd -> do   stats <- battle atkCmd
                                ST.liftIO $ print stats
                                attack

-- Fortify

fortify :: (ST.MonadState Game m, MonadIO m) => m ()
fortify = 
    do  currentPlayer <- getCurrentPlayerST
        ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", you may now fortify your units. Your territories include: "
        playerTerrs <- getPlayerTerrs =<< getCurrentPlayerST
        _ <- ST.liftIO $ sequence $ map print playerTerrs
        ST.liftIO $ putStrLn "You have two commands at your disposal:"
        ST.liftIO $ putStrLn "fortiy [SOURCE] [TARGET] [PIECES] -- Move specified number of pieces from source territory to target territory."
        ST.liftIO $ putStrLn "done -- Ends your fority phase and thus your turn."
        maybeFrtCmd <- runMaybeT getFortifyCmd
        case maybeFrtCmd of
            Nothing -> return ()
            Just (sTerr, dTerr, pcs) -> do  movePieces sTerr dTerr pcs
                                            ST.liftIO $ putStrLn $ "Moved " ++ show pcs ++ " pieces from " ++ territoryName sTerr ++ " to " ++ territoryName dTerr ++ "."
                                            fortify
        return ()

type FortifyCmd = (Territory, Territory, PieceCount)
getFortifyCmd :: (ST.MonadState Game m, MonadIO m) => MaybeT m FortifyCmd
getFortifyCmd =
    do  ST.liftIO $ putStrLn "Specify a fortify command now (or done to finish fortifying)."
        cmd <- ST.liftIO getLine
        guard (cmd /= "done")
        result <- runExceptT $ parseFortifyCmd cmd
        case result of
            Left e -> do    ST.liftIO $ putStr $ show e
                            ST.liftIO $ putStr " Please try again.\n"
                            getFortifyCmd
            Right atkCmd -> return atkCmd
        

parseFortifyCmd :: (ST.MonadState Game m, MonadError BoardError m) => String -> m FortifyCmd
parseFortifyCmd cmd =
    do  let split = words cmd
        when (length split /= 3) $ throwError $ ArityError "Needs exactly 3 arguments fool!"
        sTerr <- getTerrST $ head split
        dTerr <- getTerrST $ split !! 1
        currentPlayer <- getCurrentPlayerST
        unless (owner sTerr == Just (playerName currentPlayer)) $ throwError $ ValueError $ "You don't own source territory: " ++ territoryName sTerr ++ "."
        unless (owner dTerr == Just (playerName currentPlayer)) $ throwError $ ValueError $ "You don't own destination territory: " ++ territoryName sTerr ++ "."
        pcs <- readPieceCount $ split !! 2
        when (pcs == pieceCount sTerr) $ throwError $ ValueError "You must leave at least one piece behind."
        when (pcs > pieceCount sTerr - 1) $ throwError $ ValueError $ "You don't have " ++ show pcs ++ " many pieces at source territory: " ++ territoryName sTerr ++ "."
        when (pcs <= 0) $ throwError $ ValueError "You can't move a negative number of pieces."
        return (sTerr, dTerr, pcs)

-- Board Build Functions

insertTerritory :: Board -> TerritoryName -> ContinentName -> PieceCount -> Either BoardError Board
insertTerritory board@(Board terrs conts) newTName contName initPieces = do
        _ <- getCont board contName -- Just to ensure that the continent exists
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

rotatePlayers2 :: (ST.MonadState Game m) => m ()
rotatePlayers2 = do game <- ST.get
                    let (firstPlayer:restPlayers) = getPlayers game
                    ST.put $ game {getPlayers = restPlayers ++ [firstPlayer]}

getCurrentPlayerST :: (ST.MonadState Game m) => m Player
getCurrentPlayerST = ST.gets (head . getPlayers)

isOwner :: (MonadError BoardError m, ST.MonadState Game m) => Territory -> m Bool
isOwner terr = do   player <- getCurrentPlayerST
                    pure $ playerName player == fromMaybe "" (owner terr)

getPlayerTerrs :: (ST.MonadState Game m) => Player -> m [Territory]
getPlayerTerrs plyr = ST.gets (filter (\t -> fromMaybe "" (owner t) == playerName plyr) . territories . getBoard)

rolls :: RandomGen g => Int -> g -> ([Word], g)
rolls n g =  let results = take n $ drop 1 $ iterate (\(_, gg) -> uniformR (1, 6) gg) (0, g)
                 vals = map fst results
                 lastG = snd $ last results
            in (vals, lastG)

withRandGen :: (ST.MonadState Game m) => (a -> StdGen -> (b, StdGen)) -> a -> m b
withRandGen f a =
    do  gen <- ST.gets getRandGen
        let (result, newG) = f a gen
        ST.modify (\game -> game { getRandGen = newG })
        return result

movePieces :: (ST.MonadState Game m) => Territory -> Territory -> PieceCount -> m ()
movePieces sTerr dTerr pcs =
    do  replaceTerrST sTerr { pieceCount = pieceCount sTerr - pcs }
        replaceTerrST dTerr { pieceCount = pieceCount dTerr + pcs }

-- Stats Functions

bonusPerTerr :: Board -> ContinentName -> Either BoardError Float
bonusPerTerr board contName = do
    terrs <- terrsInCont board contName
    cont <- getCont board contName
    return $ (fromIntegral $ bonus cont) / (fromIntegral $ length terrs)

-- Utility Functions

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

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

replaceTerrST :: (ST.MonadState Game m) => Territory -> m ()
replaceTerrST terr =
    do  game <- ST.get
        let board = getBoard game
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