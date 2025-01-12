{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Objects
import Input
import Props

-- Main function + initiation of game loop
main :: IO()
main = do
    putStrLn start
    game_loop Start (player, generate_map map_size) [] 0

-- Game loop
game_loop :: State -> Scene -> History -> Time -> IO()
game_loop state scene history time = do
        putStr "> "
        line <- getLine
        let input = to_lower line
        handle_input (split ' ' input) state scene history time

-- Input handler
handle_input :: [String] -> State -> Scene -> History -> Time -> IO ()
handle_input [] state scene history time = game_loop state scene history time
handle_input (input:inputs) state scene history time = do
        let line = elongate_line input
        (st, sc, hs, tm) <- parse_input line state scene history time
        if line == "quit"
            then return ()
            else if is_dead $ fst sc
                then putStrLn death
                else handle_input inputs st sc hs tm