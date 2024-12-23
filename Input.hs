{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where
import Objects

data State = Help | Explore | Fight
    deriving (Eq, Show, Read)

to_lower :: String -> String
to_lower str = [ if is_upper x then make_lower x else x | x <- str ]
    where 
        is_upper c   = c > 'A' && c < 'Z'
        make_lower c = toEnum (fromEnum c + 32) :: Char


parse_input :: String -> State -> IO State
parse_input l st
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
    where line = to_lower l

parse_event :: Tile -> State
parse_event (E _)          = Fight
parse_event (O (Chest _))  = Explore
parse_event _              = Explore

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
