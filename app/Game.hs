{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ConstraintKinds #-}

module Game where

import Board
import Player
import System.Random
import Control.Lens
import Control.Monad.Writer
import qualified Control.Monad.State as ST

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

type GameOp a = ST.StateT Game IO a