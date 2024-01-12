{-# LANGUAGE GHC2021 #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}

import Data.List
import Data.Either
import Control.Monad.Writer
import qualified Control.Monad.State as ST
import Control.Monad.RWS
import System.Random
import Control.Lens

import Board
import Player
import GameUtils
import Game
import Turn.Deploy
import Turn.Attack
import Turn.Fortify
import Turn.Setup

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

initialBoard :: Board
initialBoard = fromRight (Board [] []) $ bulkAdd Nothing
                                          [("Asia", 7), ("North America", 5), ("South America", 2)]
                                          [ ("Kamchatka", ["Alaska"], "Asia", 1)
                                          , ("Alaska", ["Kamchatka", "Alberta"], "North America", 1)
                                          , ("Alberta", ["Alaska"], "North America", 1)
                                          , ("Peru", ["Brazil"], "South America", 1)
                                          , ("Brazil", ["Peru"], "South America", 1)]

initPlayers :: [Player]
initPlayers = [ HumanPlayer "Josh" 3 []
          , HumanPlayer "Nathan" 3 [] ]

type NewGame m a = (Monad m) => RWST () Logs Game m a

newGame :: (MonadIO m) => NewGame m ()
newGame = do    _ <- setup
                play

main :: IO ()
main = do   gen <- getStdGen
            let ng = newGame
            (_, finalGameState, (messageLog, commandLog)) <- runRWST ng () (Game initialBoard initPlayers gen)
            print $ finalGameState^.board
    
    -- cNames <- Right $ map continentName $ continents board
    -- scores <- mapM (bonusPerTerr board) cNames
    -- Right $ zip cNames scores