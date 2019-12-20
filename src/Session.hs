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

type GameResult = (Game, [String])

command :: String -> Maybe Command
command s = case reads (normalize s) of
              [(cmd,_)] -> Just cmd
              [] -> Nothing

normalize :: String -> String
normalize = unwords . capitalizeFirst . words
    where
    capitalizeFirst :: [String] -> [String]
    capitalizeFirst [] = []
    capitalizeFirst (w:ws) = capitalize w : ws

    capitalize :: String -> String
    capitalize [] = []
    capitalize (c:s) = toUpper c : map toLower s

prompt :: Monad m => (String -> m ()) -> m () 
prompt out = out "Quit | List | Go | Halt | Add \"id\" | Start \"id\" | Stop \"id\" | State \"id\" | Pos \"id\" n\n"

entry :: Monad m => (m String) -> m (Maybe Command)
entry imp = fmap command imp 

doCommand :: Game -> (Maybe Command) -> GameResult
doCommand g cmd = case cmd of
                  Just Quit -> (g, ["Bye"])
                  Just List -> (g, showAll g)
                  Just (Add playerId) -> addNewPlayer playerId g
                  Just (Start playerId) -> startPlayer playerId g
                  Just (Stop playerId) -> stopPlayer playerId g
                  Just Go -> go g
                  Just Halt -> halt g
                  Just (State playerId) -> playerState playerId g
                  Just (Pos playerId n) -> playerSetPosition playerId n g
                  Nothing -> (g,["???"])


addNewPlayer :: PlayerId -> Game -> GameResult
addNewPlayer playerId g = case stateForPlayer playerId g of
                         Left _ -> (addPlayer playerId g, ["Player " ++ playerId ++ " added to the game."])
                         Right _ -> (g, ["Player " ++ playerId ++ " is already in the game."])


startPlayer :: PlayerId -> Game -> GameResult
startPlayer playerId g = let g' = startForPlayer playerId g in
                             case stateForPlayer playerId g of
                               Right _ -> (g',["Starting simulation for "++playerId])
                               Left _ -> (g, ["Player " ++ playerId ++ " is not in the game."])

stopPlayer :: PlayerId -> Game -> GameResult
stopPlayer playerId g = let g' = stopForPlayer playerId g in
                             case stateForPlayer playerId g of
                               Right _ -> (g',["Stopping simulation for "++playerId])
                               Left _ -> (g, ["Player " ++ playerId ++ " is not in the game."])

playerState :: PlayerId -> Game -> GameResult
playerState playerId g = case stateForPlayer playerId g of
                           Right (t,p) -> (g, ["State for " ++ playerId ++ ": " ++ (show t) ++ " " ++ (show p)])
                           Left _ -> (g, ["Player " ++ playerId ++ " is not in the game."])

playerSetPosition :: PlayerId -> Position -> Game -> (Game,[String])
playerSetPosition playerId p g = let g' = setPositionForPlayer p playerId g
    in case stateForPlayer playerId g' of
         Right _ -> (g',["Player "++playerId++" set position to "++(show p)])
         Left msg -> (g, [msg])

gameLoop :: Monad m => Game -> (m String) -> (String -> m ()) -> m ()
gameLoop g inp out = do
    prompt out
    x <- entry inp    
    let (g',msg) = doCommand g x
    out (unlines msg)
    if x == Just Quit then return () else gameLoop g' inp out

go :: Game -> GameResult 
go g = (startAll g, [ "Starting all simulations" ])

halt :: Game -> GameResult
halt g = (stopAll g, ["Stopping all simulations"])
