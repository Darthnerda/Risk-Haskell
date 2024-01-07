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

import Utility
import Board
import Player
import GameUtils
import Game

-- Type Aliases





-- ADTs

data OperationCodes = NoOp
                    | CancelOp String

instance Show OperationCodes where
    show NoOp = "No operation done."
    show (CancelOp e) = "Cancelled operation due to: " ++ e ++ "."

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
            Left (CancelOp _) -> error "If you're going to cancel during setup. I might as well just blow up the game. Please try again."
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



-- Stats Functions

bonusPerTerr :: Board -> ContinentName -> Either BoardError Float
bonusPerTerr board contName = do
    terrs <- terrsInCont board contName
    cont <- getCont board contName
    return $ (fromIntegral $ bonus cont) / (fromIntegral $ length terrs)

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