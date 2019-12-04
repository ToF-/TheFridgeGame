module Room where

type Room = (Temperature, Position)
type Temperature = Double
type Position = Integer

initialTemperature :: Temperature
initialTemperature = 15.0

initialPosition :: Position
initialPosition = 100

initialRoom :: Room
initialRoom = (initialTemperature, initialPosition)

room :: Temperature -> Position -> Room
room t p = (t,p)

temperature :: Room -> Temperature
temperature = fst

position :: Room -> Position
position = snd

-- type Room = ((Temperature, Position),History)
--
-- type Temperature = Double
-- type Position = Integer
-- 
-- type History = [(Temperature,Position)]
-- 
-- initialRoom = ((15.0, 100),[])
-- 
-- temperature = fst . fst
-- position    = snd . fst
-- 
-- update :: Room -> Room
-- update ((t,p),h) = ((t + ((fromIntegral p) / 10.0 + 2.0 - temperatureAtTime 3 h) / 3.0, p), (t,p):h)
--     where
--     temperatureAtTime n h | n < length h = fst (h!!n)
--                           | otherwise    = 15
-- 
-- setPosition :: Room -> Position -> Room
-- setPosition ((t,_),h) p = ((t,p),h)
-- 
-- history :: Room -> History 
-- history = snd
