module Simulation where
import Room
import History

type Simulation = (Room,History)

initial :: Simulation
initial = (room 15.0 100, emptyHistory)

currentRoom :: Simulation -> Room
currentRoom = fst


