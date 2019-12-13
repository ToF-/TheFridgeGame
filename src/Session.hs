module Session where
import Data.Char
import Game

data Command = Quit
             | List
             | Help
             | State String
    deriving (Eq, Show, Read)

command :: String -> Maybe Command
command s = case reads (capitalize s) of
              [(cmd,_)] -> Just cmd
              [] -> Nothing

capitalize :: String -> String
capitalize [] = []
capitalize (c:s) = toUpper c : map toLower s
