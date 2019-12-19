import Session
import Game

main :: IO ()
main = gameLoop newGame getLine putStrLn
