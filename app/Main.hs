{-# LANGUAGE GHC2021 #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Writer
import qualified Control.Monad.State as ST
import Control.Monad.Except hiding (lift)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Text.Read hiding (lift)
import Data.Function
import System.Random
import Data.Ord
import Control.Lens
import Control.Exception hiding (TypeError)


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

data Board = Board { _territories :: [Territory]
                   , continents :: [Continent]} deriving Show

territories :: Lens' Board [Territory]
territories = lens _territories (\p newTerritories -> p { _territories = newTerritories })

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

data OperationCodes = NoOp
                    | CancelOp String

instance Show OperationCodes where
    show NoOp = "No operation done."
    show (CancelOp e) = "Cancelled operation due to: " ++ e ++ "."

data Card = Horse
          | Soldier
          | Cannon
          | Wildcard deriving (Eq)

data Player = HumanPlayer { playerName :: PlayerName
                          , unitDeployment :: PieceCount
                          , bonusCards :: [Card] } deriving (Eq)

-- Game State Monad
-- newtype Game = Game { getGame :: (Board, [Player], StdGen) }
data Game = Game { _board :: Board 
                 , _players :: [Player]
                 , getRandGen :: StdGen }

board :: Lens' Game Board
board = lens _board (\p newBoard -> p { _board = newBoard })

players :: Lens' Game [Player]
players = lens _players (\p newPlayers -> p { _players = newPlayers })

type MsgLog = [String]
type CmdLog = [Command]

type Logs = (MsgLog, CmdLog)

tellMsg :: (MonadWriter Logs m) => MsgLog -> m ()
tellMsg xs = tell (xs, mempty)

tellCmd :: (MonadWriter Logs m) => CmdLog -> m ()
tellCmd xs = tell (mempty, xs)

-- type Logs m =  (MonadWriter MsgLog m, MonadWriter CmdLog m)

-- newtype GameOp a = GameOp { runGame :: ST.StateT Game IO a }
type GameOp a = ST.StateT Game IO a


data Command    = SetupCmd Player Territory
                | DeployCmd Territory PieceCount
                | AttackCmd Territory Territory PieceCount
                | FortifyCmd Territory Territory PieceCount
                | SurrenderCmd Player

instance Show Command where
    show (SetupCmd plyr terr) = playerName plyr ++ " chooses " ++ territoryName terr ++ "."
    show (DeployCmd terr pc) = "Player deploys " ++ show pc ++ " pieces to " ++ territoryName terr ++ "."
    show (AttackCmd sTerr dTerr pc) = "Player attacks from " ++ territoryName sTerr ++ " to " ++ territoryName dTerr ++ " with " ++ show pc ++ " pieces."
    show (FortifyCmd sTerr dTerr pc) = "Player moves " ++ show pc ++ " pieces from " ++ territoryName sTerr ++ " to " ++ territoryName dTerr ++ "."
    show (SurrenderCmd plyr) = playerName plyr ++ "surrenders."

-- Game Functions

-- Command interception

playerPrompt :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => ExceptT OperationCodes m String
playerPrompt =
    do  msg <- ST.liftIO getLine
        let cmd = break (==' ') msg
        case cmd of
            ([],[]) -> throwError NoOp
            (opr,args) -> case opr of
                            "surrender" -> do   surrender
                                                throwError $ CancelOp "Player surrendered"
                            "myterrs" -> do printMyTerrs
                                            playerPrompt
                            _ -> return msg

printMyTerrs :: (ST.MonadState Game m, MonadIO m) => m ()
printMyTerrs = do   currentPlayer <- getCurrentPlayerST
                    pTerrs <- getPlayerTerrs currentPlayer
                    ST.liftIO $ mapM_ print pTerrs


surrender :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m ()
surrender = do  currentPlayer <- getCurrentPlayerST
                board.territories.each.filtered (\t -> owner t == Just (playerName currentPlayer)) %= (\t -> t { owner = Nothing }) -- for each territory where owner is the current player, changes its owner to Nothing.
                modifying players (delete currentPlayer) -- zap the player from the players list
                let surrenderMsg = playerName currentPlayer ++ " surrenders."
                ST.liftIO $ putStrLn surrenderMsg
                tellMsg [surrenderMsg]
                tellCmd [SurrenderCmd currentPlayer]


-- Setup

getSetupCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m Command
getSetupCmd =
    do game <- ST.get
       let brd = game^.board
       currentPlayer <- getCurrentPlayerST
       ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", please choose a territory."
       terrChoiceResult <- runExceptT playerPrompt
       case terrChoiceResult of
            Left NoOp -> getSetupCmd
            Left (CancelOp e) -> error "If you're going to cancel during setup. I might as well just blow up the game. Please try again."
            Right terrChoice -> case getTerr brd terrChoice of
                                    Left e -> do ST.liftIO $ putStrLn $ show e ++ " Please try again."
                                                 getSetupCmd
                                    Right (Territory tn _ _ _ (Just previousOwner) _) -> do ST.liftIO $ putStrLn $ tn ++ " is already owned by " ++ previousOwner ++ ". Please choose a different territory."
                                                                                            getSetupCmd
                                    Right t -> return (SetupCmd currentPlayer t)

setup :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m Board
setup =
    do  brd <- use board
        let terrsLeft = brd^.territories.to (any (isNothing . owner))
        if terrsLeft then do
            setupCmd <- getSetupCmd
            let (plyr, terr) = case setupCmd of
                            (SetupCmd p t) -> (p, t)
                            _ -> error "Received something other than a SetupCmd in setup."
            replaceTerrST terr { owner = Just $ playerName plyr }
            tellMsg [show setupCmd]
            tellCmd [setupCmd]
            rotatePlayers2
            setup
        else
            use board

-- Play

play :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m ()
play = do
    brd <- use board
    let playerHoldings = brd^.territories.to (groupBy (\a b -> owner a == owner b))
    case playerHoldings of
        [(Territory _ _ _ _ (Just winnerName) _):_] -> ST.liftIO $ putStrLn $ "Congratulations " ++ winnerName ++ ". You won!" -- only matches if exactly one player
        _ -> do ST.liftIO $ putStrLn "A new turn!"
                tellMsg ["Began new turn."]
                currentPlayer <- getCurrentPlayerST
                deploy $ unitDeployment currentPlayer
                attack
                fortify
                rotatePlayers2
                play
        

-- Player Functions

-- Deploy

deploy :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => PieceCount -> m ()
deploy 0 = return ()
deploy deployRemaining =
    do deployCmd <- runMaybeT $ getDeployCmd deployRemaining
       case deployCmd of
            Nothing -> return ()
            Just dc@(DeployCmd terr dply) -> do deployToBoard dc
                                                tellMsg [show dc]
                                                tellCmd [dc]
                                                deploy (deployRemaining - dply)
            _ -> error "Received something other than a DeployCmd in the deploy function."
       
    
deployToBoard :: (ST.MonadState Game m) => Command -> m ()
deployToBoard (DeployCmd terr dply) = replaceTerrST $ terr { pieceCount = dply + pieceCount terr }
deployToBoard _ = pure ()

promptDeployCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => MaybeT m String
promptDeployCmd =
    do  deployChoiceResult <- runExceptT playerPrompt
        case deployChoiceResult of
            Left NoOp -> promptDeployCmd
            Left (CancelOp e) -> liftMaybe Nothing
            Right s -> return s

getDeployCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => PieceCount -> MaybeT m Command
getDeployCmd deployRemaining =
    do currentPlayer <- getCurrentPlayerST
       ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", you have " ++ show deployRemaining ++ " deployment remaining. Type a territory name and the number of units you would like to deploy there."
       userInput <- promptDeployCmd
       result <- runExceptT $ parseDeployCmd userInput deployRemaining
       case result of
            Left err -> (do ST.liftIO $ print err
                            getDeployCmd deployRemaining)
            Right deployCmd -> return deployCmd

parseDeployCmd :: (MonadError BoardError m, ST.MonadState Game m) => String -> PieceCount -> m Command
parseDeployCmd cmd deployRemaining = 
    do  let splt = words cmd
        when (length splt /= 2) $ throwError $ ArityError "Needs exactly 2 arguments fool."
        terr <- getTerrST $ head splt
        player <- use $ players.to head
        when (playerName player /= fromMaybe "" (owner terr)) $ throwError $ ValueError "That's not your territory! Pick another you fool!"
        pcs <- readPieceCount $ splt !! 1
        when (pcs < 0) $ throwError $ ValueError "You can't deploy less than 0."
        when (pcs > deployRemaining) $ throwError $ ValueError "You can't deploy more than youre remaining deployment."
        pure (DeployCmd terr pcs)

-- Attack

parseAttackCmd :: (MonadError BoardError m, ST.MonadState Game m) => String -> m Command
parseAttackCmd cmd = 
    do  let splt = words cmd
        when (length splt /= 3) $ throwError $ ArityError "Needs exactly 3 arguments fool."
        sTerr <- getTerrST $ head splt
        dTerr <- getTerrST $ splt !! 1
        ownS <- isOwner sTerr
        ownD <- isOwner dTerr
        unless ownS $ throwError $ ValueError "That's not your territory! Pick another you fool!"
        when ownD $ throwError $ ValueError "You can't attack your own territory. Wait until the fortify step to move units."
        pcs <- readPieceCount $ splt !! 2
        when (pcs > pieceCount sTerr - 1) $ throwError $ ValueError "You can't attack with more pieces than you have on the territory minus one."
        when (pcs < 1) $ throwError $ ValueError "You must attack with at least one piece."
        pure (AttackCmd sTerr dTerr pcs)

promptAttackCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => MaybeT m String
promptAttackCmd =
    do  attackCmdResult <- runExceptT playerPrompt
        case attackCmdResult of
            Left NoOp -> promptAttackCmd
            Left (CancelOp e) -> liftMaybe Nothing
            Right cmd -> return cmd

getAttackCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => MaybeT m Command
getAttackCmd =
    do  ST.liftIO $ putStrLn "Type your command now. Choose wisely."
        cmd <- promptAttackCmd
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

battle :: (ST.MonadState Game m) => Command -> m BattleStats
battle (AttackCmd sTerr dTerr apc) =
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
battle _ = error "Battle received a different kind of command than an AttackCmd."


attack :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m ()
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
            Just atkCmd -> do   tellMsg [show atkCmd]
                                tellCmd [atkCmd]
                                stats <- battle atkCmd
                                ST.liftIO $ print stats
                                attack

-- Fortify

fortify :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m ()
fortify = 
    do  currentPlayer <- getCurrentPlayerST
        ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", you may now fortify your units. Your territories include: "
        playerTerrs <- getPlayerTerrs =<< getCurrentPlayerST
        _ <- ST.liftIO $ sequence $ map print playerTerrs
        ST.liftIO $ putStrLn "You have two commands at your disposal:"
        ST.liftIO $ putStrLn "fortify [SOURCE] [TARGET] [PIECES] -- Move specified number of pieces from source territory to target territory."
        ST.liftIO $ putStrLn "done -- Ends your fority phase and thus your turn."
        maybeFrtCmd <- runMaybeT getFortifyCmd
        case maybeFrtCmd of
            Nothing -> return ()
            Just frtCmd@(FortifyCmd sTerr dTerr pcs) -> do  movePieces sTerr dTerr pcs
                                                            ST.liftIO $ putStrLn $ "Moved " ++ show pcs ++ " pieces from " ++ territoryName sTerr ++ " to " ++ territoryName dTerr ++ "."
                                                            tellMsg [show frtCmd]
                                                            tellCmd [frtCmd]
                                                            fortify
            Just _ -> error "Somehow managed to get something other than a FortifyCmd in the fortify function."
        return ()

promptFortifyCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => MaybeT m String
promptFortifyCmd =
    do  fortifyResult <- runExceptT playerPrompt
        case fortifyResult of
            Left NoOp -> promptFortifyCmd
            Left (CancelOp e) -> liftMaybe Nothing
            Right f -> return f

getFortifyCmd :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => MaybeT m Command
getFortifyCmd =
    do  ST.liftIO $ putStrLn "Specify a fortify command now (or done to finish fortifying)."
        cmd <- promptFortifyCmd
        guard (cmd /= "done")
        result <- runExceptT $ parseFortifyCmd cmd
        case result of
            Left e -> do    ST.liftIO $ putStr $ show e
                            ST.liftIO $ putStr " Please try again.\n"
                            getFortifyCmd
            Right atkCmd -> return atkCmd
        

parseFortifyCmd :: (ST.MonadState Game m, MonadError BoardError m) => String -> m Command
parseFortifyCmd cmd =
    do  let splt = words cmd
        when (length splt /= 3) $ throwError $ ArityError "Needs exactly 3 arguments fool!"
        sTerr <- getTerrST $ head splt
        dTerr <- getTerrST $ splt !! 1
        currentPlayer <- getCurrentPlayerST
        unless (owner sTerr == Just (playerName currentPlayer)) $ throwError $ ValueError $ "You don't own source territory: " ++ territoryName sTerr ++ "."
        unless (owner dTerr == Just (playerName currentPlayer)) $ throwError $ ValueError $ "You don't own destination territory: " ++ territoryName sTerr ++ "."
        pcs <- readPieceCount $ splt !! 2
        when (pcs == pieceCount sTerr) $ throwError $ ValueError "You must leave at least one piece behind."
        when (pcs > pieceCount sTerr - 1) $ throwError $ ValueError $ "You don't have " ++ show pcs ++ " many pieces at source territory: " ++ territoryName sTerr ++ "."
        when (pcs <= 0) $ throwError $ ValueError "You can't move a negative number of pieces."
        return (FortifyCmd sTerr dTerr pcs)

-- Board Build Functions

insertTerritory :: Board -> TerritoryName -> ContinentName -> PieceCount -> Either BoardError Board
insertTerritory brd@(Board terrs conts) newTName contName initPieces = do
        _ <- getCont brd contName -- Just to ensure that the continent exists
        ( let newTerritory = Territory newTName [] contName initPieces Nothing initPieces
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
getTerr board terrName = case board^.territories.to (find ((terrName ==) . territoryName)) of
    Just terr -> Right terr
    Nothing -> Left $ NoSuchTerritory terrName

getTerrST :: (MonadError BoardError m, ST.MonadState Game m) => TerritoryName -> m Territory
getTerrST terrName = do terrs <- use $ board.territories
                        case find ((terrName ==) . territoryName) terrs of
                            Just terr -> pure terr
                            Nothing -> throwError $ NoSuchTerritory terrName

readPieceCount :: (MonadError BoardError m, ST.MonadState Game m) => String -> m PieceCount
readPieceCount pc = case readMaybe pc of
                        (Just pcp) -> pure pcp
                        Nothing -> throwError $ ValueError "Malformed piece count argument."

terrsInCont :: Board -> ContinentName -> Either BoardError [Territory]
terrsInCont board contName = if contName `elem` contNames
    then Right $ board^.territories.to (filter ((contName ==) . continent))
    else Left $ NoSuchContinent contName
    where contNames = map continentName $ continents board

rotatePlayers2 :: (ST.MonadState Game m, MonadWriter Logs m) => m ()
rotatePlayers2 = do game <- ST.get
                    let (firstPlayer:restPlayers) = game^.players
                    players .= restPlayers ++ [firstPlayer]
                    -- ST.put $ game {getPlayers = restPlayers ++ [firstPlayer]}
                    tellMsg ["Next player's turn: " ++ playerName (head restPlayers) ++ "."]

getCurrentPlayerST :: (ST.MonadState Game m) => m Player
getCurrentPlayerST = use $ players.to head

isOwner :: (MonadError BoardError m, ST.MonadState Game m) => Territory -> m Bool
isOwner terr = do   player <- getCurrentPlayerST
                    pure $ playerName player == fromMaybe "" (owner terr)

getPlayerTerrs :: (ST.MonadState Game m) => Player -> m [Territory]
getPlayerTerrs plyr = use $ board.territories.to (filter (\t -> fromMaybe "" (owner t) == playerName plyr))

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

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

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

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act init =
   when (test init) $ act init >>= whileM test act

replaceTerritory :: Territory -> GameOp ()
replaceTerritory terr = do game <- ST.get
                           let  terrs = game^.board.territories
                                newTerrs = replaceOn ((==) `on` territoryName) terr terrs
                           board.territories .= newTerrs
                        --    ST.put $ game { getBoard =  board { territories = newTerrs }}
                           return ()

replaceTerrST :: (ST.MonadState Game m) => Territory -> m ()
replaceTerrST terr =
    do  game <- ST.get
        let terrs = game^.board.territories
            newTerrs = replaceOn ((==) `on` territoryName) terr terrs
        board.territories .= newTerrs
        -- ST.put $ game { board =  board { territories = newTerrs }}
        return ()

initialBoard = fromRight (Board [] []) $ bulkAdd Nothing
                                          [("Asia", 7), ("North America", 5), ("South America", 2)]
                                          [ ("Kamchatka", ["Alaska"], "Asia", 1)
                                          , ("Alaska", ["Kamchatka", "Alberta"], "North America", 1)
                                          , ("Alberta", ["Alaska"], "North America", 1)
                                          , ("Peru", ["Brazil"], "South America", 1)
                                          , ("Brazil", ["Peru"], "South America", 1)]

initPlayers = [ HumanPlayer "Josh" 3 []
          , HumanPlayer "Nathan" 3 [] ]

-- main = runGame $ ( do { setupBoard <- setup; play; ST.liftIO $ print setupBoard } ) (Game )

type NewGame m a = (Monad m) => RWST () Logs Game m a

newGame :: (MonadIO m) => NewGame m ()
newGame = do    _ <- setup
                play

-- runGame :: WriterT CmdLog (WriterT MsgLog (ST.StateT Game IO)) b -> Game -> (a, Game, CmdLog, MsgLog)
-- runGame :: Monad m => NewGame m a -> () -> Game -> m (a, Game, (CmdLog, MsgLog))
-- runGame action initialGame = runRWST action initialGame


-- runGame action initialGame = do
--   -- Unwrap the outermost layer using runStateT
--   (result, cmdLog, msgLog) <- ST.runStateT (runWriterT (runWriterT action)) initialGame
--   return (result, initialGame, cmdLog, msgLog)

main :: IO ()
main = do   gen <- getStdGen
            let ng = newGame
            (_, finalGameState, (messageLog, commandLog)) <- runRWST ng () (Game initialBoard initPlayers gen)
            print $ finalGameState^.board
    
    -- cNames <- Right $ map continentName $ continents board
    -- scores <- mapM (bonusPerTerr board) cNames
    -- Right $ zip cNames scores