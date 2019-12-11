module Game where

import Simulation
import Room

type Game = [(PlayerId,SimulationState)]
type PlayerId = String

newGame :: Game
newGame = []

simulationFor :: PlayerId -> Game -> SimulationState
simulationFor p g = case lookup p g of
                      Just s -> s
                      Nothing -> Left ("NO EXISTING SIMULATION FOR: " ++ p)

addPlayer :: PlayerId -> Game -> Game
addPlayer p = ((p,newSimulation):)

stateForPlayer :: PlayerId -> Game -> Either String RoomState
stateForPlayer p g = fmap (state . currentRoom) (simulationFor p g)
