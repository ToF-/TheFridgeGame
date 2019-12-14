module Session where
import Data.Char
import Room
import Game
import Simulation

data Command = Quit
             | List
             | Help
             | State PlayerId
             | Pos PlayerId Position 
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

sessionLoop :: Monad m => Game -> (m String) -> (String -> m ()) -> m ()
sessionLoop g imp out = do
    prompt out
    cmd <- entry imp
    case cmd of
      Just Quit -> out "Bye"
      Nothing -> do
          out "???"
          sessionLoop g imp out

