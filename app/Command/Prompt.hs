{-# LANGUAGE GHC2021 #-}
module Command.Prompt where

import Game
import Command.Op
import Board
import Player
import GameUtils

import qualified Control.Monad.State as ST
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Except hiding (lift)

import Control.Lens
import Data.List

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
                            "mycards" -> do printMyCards
                                            playerPrompt
                            _ -> return msg

printMyCards :: (ST.MonadState Game m, MonadIO m) => m ()
printMyCards = do   currentPlayer <- getCurrentPlayerST
                    ST.liftIO $ mapM_ print $ bonusCards currentPlayer

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