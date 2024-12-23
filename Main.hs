{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Objects
import Input
import Props

main :: IO()
main = do
    putStrLn start 
    game_loop 0 Explore (player, generate_map map_size) []

game_loop :: Time -> State -> Scene -> History -> IO()
game_loop time state scene@(actor, world) history = do
        putStr "> "
        line <- getLine
        let input = to_lower line
        (st, sc, hs) <- parse_input input state scene history
        if input == "quit"
            then return ()
            else game_loop (time + 1) st sc hs

