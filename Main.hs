{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Objects
import Input
import Props

main :: IO()
main = do
    putStrLn start 
    game_loop Explore (player, generate_map map_size) [] 0

game_loop :: State -> Scene -> History -> Time -> IO()
game_loop state scene@(actor, world) history time = do
        putStr "> "
        line <- getLine
        let input = to_lower line
        (st, sc, hs, tm) <- parse_input input state scene history time
        if input == "quit"
            then return ()
            else game_loop st sc hs tm

