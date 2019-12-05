module Simulation where
import Room
import History

data Simulation = Simulation { 
    currentRoom :: Room,
    history :: History,
    status :: Status }
    deriving (Eq, Show)

data Status = Idle | Running
    deriving (Eq, Show)

initial :: Simulation
initial = Simulation 
    (room 15.0 100)
    emptyHistory
    Idle 

update :: Simulation -> Simulation
update s | status s == Idle = s
         | otherwise = s { 
                currentRoom = currentRoom',
                history = record (currentRoom s) (history s) }
    where 
    currentRoom' = room (temp + delta) pos
        where
        delta = ((fromIntegral pos) / 10.0 + 2.0 - temperatureAtTime (-4)) / 3.0
        temp  = temperature (currentRoom s)
        pos   = position (currentRoom s)
        temperatureAtTime t = temperature ((history s) `at` t)

updateRoom :: Simulation -> Either String Simulation
updateRoom s | status s == Running = Right $ update s
updateRoom _ = Left "SIMULATION NOT RUNNING"

setPosition :: Position -> Simulation -> Either String Simulation
setPosition p s | status s == Running = Right $ s { currentRoom = (t,p) }
    where
    (t,_) = currentRoom s 
setPosition _ _ = Left "SIMULATION NOT RUNNING"

start :: Simulation -> Either String Simulation
start s = Right $ s { status = Running }

stop :: Simulation -> Either String Simulation
stop s = Right $ s { status = Idle }
