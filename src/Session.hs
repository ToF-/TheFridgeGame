module Session where
import Data.Char
import Room
import Game
import Simulation

data Command = Quit
             | List
             | Help
             | Add PlayerId
             | State PlayerId
             | Pos PlayerId Position 
             | Go
             | Halt
             | Start PlayerId
             | Stop PlayerId
    deriving (Eq, Show, Read)

command :: String -> Maybe Command
command s = case reads (capitalize s) of
              [(cmd,_)] -> Just cmd
              [] -> Nothing

capitalize :: String -> String
capitalize [] = []
capitalize (c:s) = toUpper c : map toLower s

prompt :: Monad m => (String -> m ()) -> m () 
prompt out = out "Quit | List | Help | State \"id\" | Pos \"id\" n\n"

entry :: Monad m => (m String) -> m (Maybe Command)
entry imp = fmap command imp 

doCommand :: Game -> String -> (Game, [String])
doCommand g s = case command s of
                  Just Quit -> (g,["Bye"])
                  Just (Add playerId) -> addNewPlayer playerId g
                  Just (State playerId) -> playerState playerId g
                  Just (Pos playerId n) -> playerSetPosition playerId n g
                  Nothing -> (g,["???"])

addNewPlayer :: PlayerId -> Game -> (Game, [String])
addNewPlayer playerId g = case stateForPlayer playerId g of
                         Left _ -> (addPlayer playerId g, ["Player " ++ playerId ++ " added to the game."])
                         Right _ -> (g, ["Player " ++ playerId ++ " is already in the game."])


playerState :: PlayerId -> Game -> (Game, [String])
playerState playerId g = case stateForPlayer playerId g of
                           Right (t,p) -> (g, ["State for " ++ playerId ++ ": " ++ (show t) ++ " " ++ (show p)])
                           Left _ -> (g, ["Player " ++ playerId ++ " is not in the game."])

playerSetPosition :: PlayerId -> Position -> Game -> (Game,[String])
playerSetPosition playerId p g = let g' = setPositionForPlayer p playerId g
    in case stateForPlayer playerId g' of
         Right _ -> (g',["Player "++playerId++" set position to "++(show p)])
         Left msg -> (g, [msg])
                                   
