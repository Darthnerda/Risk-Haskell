{-# LANGUAGE GHC2021 #-}
module Turn.Fortify where

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

fortify :: (ST.MonadState Game m, MonadIO m, MonadWriter Logs m) => m ()
fortify = 
    do  currentPlayer <- getCurrentPlayerST
        ST.liftIO $ putStrLn $ playerName currentPlayer ++ ", you may now fortify your units. Your territories include: "
        playerTerrs <- getPlayerTerrs =<< getCurrentPlayerST
        _ <- ST.liftIO $ mapM print playerTerrs
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