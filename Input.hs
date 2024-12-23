{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where

data State = Help | Explore | Fight
    deriving (Eq, Show, Read)

parse_input :: String -> State -> IO State
parse_input line st
    | line == "quit"= do 
                        putStrLn "Game Exited" 
                        return st
    | st == Explore = 
        case line of
            "help" -> help
            _      -> invalid_input st
    | st == Fight   = 
        case line of
            "help" -> help
            _      -> invalid_input st
    | st == Help    = 
        case line of
            "combat" -> help_combat
            _        -> help
    | otherwise     = error "Invalid State"
                    

help :: IO State
help = do
        putStrLn label
        return Help
        where 
            label = "()================================()\n"
                ++ "|| _     _ _______         _____  ||\n"
                ++ "|| |_____| |______ |      |_____] ||\n"
                ++ "|| |     | |______ |_____ |       ||\n"
                ++ "||                                ||\n"
                ++ "()================================()\n"
                ++ " | What do you need help with?    | \n"
                ++ " | - Combat                       | \n"
                ++ " | - Exploration                  | \n"
                ++ " | - Controls                     | \n"
                ++ " | - Exit                         | \n"
                ++ " +--------------------------------+ \n"

help_combat :: IO State
help_combat = do
                putStrLn label
                return Help
                where
                    label = " Combat "
                    

invalid_input :: State -> IO State
invalid_input st = do
            putStrLn "Invalid Input!"
            return st