{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ConstraintKinds #-}

module GameUtils where

import Board
import Player
import Game

import Control.Monad.Except hiding (lift)
import qualified Control.Monad.State as ST
import Control.Monad.Writer
import Control.Lens
import Data.Maybe
import Data.List
import Text.Read
import Utility
import Data.Function
import System.Random

getTerrST :: (MonadError BoardError m, ST.MonadState Game m) => TerritoryName -> m Territory
getTerrST terrName = do terrs <- use $ board.territories
                        case find ((terrName ==) . territoryName) terrs of
                            Just terr -> pure terr
                            Nothing -> throwError $ NoSuchTerritory terrName

readPieceCount :: (MonadError BoardError m) => String -> m PieceCount
readPieceCount pc = case readMaybe pc of
                        (Just pcp) -> pure pcp
                        Nothing -> throwError $ ValueError "Malformed piece count argument."



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



withRandGen :: (ST.MonadState Game m) => (a -> StdGen -> (b, StdGen)) -> a -> m b
withRandGen f a =
    do  gen <- ST.gets getRandGen
        let (result, newG) = f a gen
        ST.modify (\game -> game { getRandGen = newG })
        return result

replaceTerrST :: (ST.MonadState Game m) => Territory -> m ()
replaceTerrST terr =
    do  game <- ST.get
        let terrs = game^.board.territories
            newTerrs = replaceOn ((==) `on` territoryName) terr terrs
        board.territories .= newTerrs
        -- ST.put $ game { board =  board { territories = newTerrs }}
        return ()

movePieces :: (ST.MonadState Game m) => Territory -> Territory -> PieceCount -> m ()
movePieces sTerr dTerr pcs =
    do  replaceTerrST sTerr { pieceCount = pieceCount sTerr - pcs }
        replaceTerrST dTerr { pieceCount = pieceCount dTerr + pcs }