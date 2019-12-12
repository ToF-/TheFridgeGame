module Game where

import Simulation
import Room
import Data.Map as M

type Game = Map PlayerId SimulationState
type PlayerId = String

newGame :: Game
newGame = M.empty

simulationFor :: PlayerId -> Game -> SimulationState
simulationFor p g = case M.lookup p g of
                      Just s -> s
                      Nothing -> Left ("NO EXISTING SIMULATION FOR: " ++ p)

addPlayer :: PlayerId -> Game -> Game
addPlayer p g = M.insert p newSimulation g

setPositionForPlayer :: Position -> PlayerId -> Game -> Game
setPositionForPlayer n p g = case simulationFor p g of
                             Right st -> M.insert p (Right st >>= setPosition n) g
                             _ -> g
                            

stateForPlayer :: PlayerId -> Game -> Either String RoomState
stateForPlayer p g = fmap (state . currentRoom) (simulationFor p g)

startForPlayer :: PlayerId -> Game -> Game
startForPlayer p g = case simulationFor p g of
                       Right st -> M.insert p (Right st >>= start) g
                       _ -> g

stopForPlayer :: PlayerId -> Game -> Game
stopForPlayer p g = case simulationFor p g of
                      Right st -> M.insert p (Right st >>= stop) g
                      _  -> g
                       
updateRunningSimulations :: Game -> Game
updateRunningSimulations = M.map (>>= Simulation.update) 

startAll :: Game -> Game
startAll = M.map (>>= start)

stopAll :: Game -> Game
stopAll = M.map (>>= stop)
