{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where
import Objects

data State = Start | Help | Explore | Fight
    deriving (Eq, Show, Read)

parse_input :: String -> State -> Scene -> History -> IO (State, Scene, History)
parse_input line st sc@(pl, wd) hs
    | line == "quit"= output "Game Exited!" ret
    | line == "log" = output (foldr (\el res ->el ++ " " ++ res) "" hs) ret
    | st == Explore =
        case line of
            "help"  -> output help (Help, sc, hlog)
            "left"  -> move_player (-1, 0) ret_log
            "right" -> move_player (1, 0) ret_log
            "up"    -> move_player (0, -1) ret_log
            "down"  -> move_player (0, 1) ret_log
            _       -> output invalid_input ret
    | st == Fight   =
        case line of
            _      -> output invalid_input ret
    | st == Help    =
        case line of
            "combat" -> output help_combat (Help, sc, hlog)
            "exit"   -> do
                          redraw_room sc
                          return (Explore, sc, hs)
            _        -> do
                          r <- output help (Help, sc, hs)
                          output invalid_input r
    | st == Start    =
        case line of
            "start" -> output "Game Started!" (Explore, sc, hs)
            _       -> output invalid_input (Start, sc, hs)
    | otherwise     = error "Invalid State"
    where
        ret     = (st, sc, hs)
        ret_log = (st, sc, hlog)
        hlog    = line : hs

parse_event :: Tile -> State
parse_event (E _)          = Fight
parse_event (O (Chest _))  = Explore
parse_event _              = Explore

output :: String -> (State, Scene, History) -> IO (State, Scene, History)
output line sc = do
                   putStrLn line
                   return sc

move_player :: Coords -> (State, Scene, History) -> IO (State, Scene, History)
move_player dir (st, sc@(pl, wd), hs) = do
                         let player = move dir pl wd
                         redraw_room (player, wd)
                         if position pl == position player
                            then putStrLn "You can't go there!"
                            else putStr ""
                         return (st, (player, wd), hs)


redraw_room :: Scene -> IO()
redraw_room (pl, wd) = do
                        print_room pl $ get_room (fst $ position pl) wd

print_room :: Entity -> Room -> IO()
print_room pl room = do
                putStrLn $ concat [ concat 
                                    [ el ++ " " | (x, obj) <- row, 
                                                        let el = if (x, y) == pos 
                                                                    then show tpl else show obj] 
                                    ++ ['\n'] | (y, row) <- indexed ]
                where 
                    indexed = zip [0..] (map (zip [0..]) room)
                    pos     = snd $ position pl
                    tpl     = E pl

to_lower :: String -> String
to_lower str = [ if is_upper x then make_lower x else x | x <- str ]
    where
        is_upper c   = c > 'A' && c < 'Z'
        make_lower c = toEnum (fromEnum c + 32) :: Char

start :: String
start = "Hello, adventurer! To play the game type start!"

help :: String
help = "()================================()\n"
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

help_combat :: String
help_combat = " Combat "


invalid_input :: String
invalid_input = "Invalid Input!"
