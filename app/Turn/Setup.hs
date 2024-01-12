{-# LANGUAGE GHC2021 #-}
module Turn.Setup where

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