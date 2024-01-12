{-# LANGUAGE GHC2021 #-}

module Turn.Deploy where

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

import Data.Maybe

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
       ST.liftIO $ putStrLn "If you would like to use cards, type:"
       ST.liftIO $ putStrLn "'cards' followed by a list of the cards you wish to use, such as: 'horse cannon soldier wildcard'."
       userInput <- promptDeployCmd
       if isPrefixOf "cards" userInput then

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

consumeBonusCards :: (MonadError BoardError m, ST.MonadState Game m) => String -> m PieceCount
consumeBonusCards cmd =
    do  let splt = words cmd
        when (length splt == 0) $ error "This shouldn't happen."