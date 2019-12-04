module Simulation where
import Room
import History

type Simulation = (Room,History)

initial :: Simulation
initial = (room 15.0 100, emptyHistory)

currentRoom :: Simulation -> Room
currentRoom = fst

update :: Simulation -> Simulation
update (r,h) = (evolve r h, record r h)
    where 

    evolve r h =  room (temp + delta) pos 
        where
        delta = ((fromIntegral pos) / 10.0 + 2.0 - temperatureAtTime (-4)) / 3.0
        temp  = temperature r
        pos   = position r
        temperatureAtTime t = temperature (h `at` t)

