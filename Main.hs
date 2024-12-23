{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Objects
import Input

main :: IO()
main = do
    game_loop 0 Explore

game_loop :: Time -> State -> IO()
game_loop time state = do
        input <- getLine
        -- Clear console
        putStr "\ESC[2J"
        st    <- parse_input input state
        if input == "quit"
            then return ()
            else game_loop (time + 1) st

player :: Entity
player = Player 10 3 1 [] [] ((0, 0), (0, 0))