{-# LANGUAGE GHC2021 #-}
module Turn.Attack where

import Game
import Command.Op
import Command.Prompt
import Player
import Board
import GameUtils
import Utility


import qualified Control.Monad.State as ST
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Except hiding (lift)
import Control.Lens

import Data.List
import Data.Ord

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
        _ <- ST.liftIO $ mapM print playerTerrs
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