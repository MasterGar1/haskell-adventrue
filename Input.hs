{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where
import Objects

data State = Start | Help | Explore | Fight
    deriving (Eq, Show, Read)

type GameData = (State, Scene, History, Time)

parse_input :: String -> State -> Scene -> History -> Time -> IO GameData
parse_input line st sc@(pl, wd) hs tm
    | line == "quit"= output "Game Exited!" ret
    | line == "log" = output (foldr (\el res ->el ++ " " ++ res) "" hs) ret
    | st == Explore =
        case line of
            "help"  -> output help (Help, sc, hlog, time)
            "left"  -> move_player (-1, 0) ret_log
            "right" -> move_player (1, 0) ret_log
            "up"    -> move_player (0, -1) ret_log
            "down"  -> move_player (0, 1) ret_log
            _       -> output invalid_input ret
    | st == Fight   =
        case line of
            _      -> output "Fight" ret
    | st == Help    =
        case line of
            "combat"  -> output help_combat (Help, sc, hlog, time)
            "explore" -> output help_explore (Help, sc, hlog, time)
            "exit"    -> do
                          redraw_room sc
                          return (Explore, sc, hs, time)
            _         -> do
                          r <- output help (Help, sc, hs, time)
                          output invalid_input r
    | st == Start    =
        case line of
            "start" -> do
                        r <- output "Game Started!" (Explore, sc, hs, time)
                        redraw_room sc
                        return r
            _       -> output invalid_input (Start, sc, hs, time)
    | otherwise     = error "Invalid State"
    where
        time    = tm + 1
        ret     = (st, sc, hs, time)
        ret_log = (st, sc, hlog, time)
        hlog    = line : hs

parse_event :: Tile -> State
parse_event (E _) = Fight
parse_event _     = Explore

output :: String -> GameData -> IO GameData
output line sc = do
                   putStrLn line
                   return sc

move_player :: Coords -> GameData -> IO GameData
move_player dir (st, sc@(pl, wd), hs, tm) = do
                         let player = move dir pl wd
                         let state = parse_event $ current_tile (position player) wd
                         if state == Fight
                            then 
                                output (combat_screen (current_tile (position player) wd) player) (state, (player, wd), hs, tm)
                            else do
                                redraw_room (player, wd)
                                if position pl == position player
                                    then putStrLn "You can't go there!"
                                    else putStr ""
                                return (state, (player, wd), hs, tm)


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
    ++ " | - Explore                      | \n"
    ++ " | - Controls                     | \n"
    ++ " | - Exit                         | \n"
    ++ " +--------------------------------+ \n"

help_combat :: String
help_combat = "()=================================================()\n"  
           ++ "|| _______  _____  _______ ______  _______ _______ ||\n"
           ++ "|| |       |     | |  |  | |_____] |_____|    |    ||\n" 
           ++ "|| |_____  |_____| |  |  | |_____] |     |    |    ||\n"
           ++ "||                                                 ||\n"
           ++ "()=================================================()\n"
                
help_explore :: String
help_explore = "()========================================================()\n"   
            ++ "|| _______ _     _  _____          _____   ______ _______ ||\n"
            ++ "|| |______  \\___/  |_____] |      |     | |_____/ |______ ||\n"
            ++ "|| |______ _/   \\_ |       |_____ |_____| |    \\_ |______ ||\n"
            ++ "||                                                        ||\n"
            ++ "()========================================================()\n"

combat_screen :: Tile -> Entity -> String
combat_screen (E (Enemy hp atk def sk name)) (Player hpp atkp defp skp _ _) = 
               "()========================================================()\n"
            ++ "||          _______ _____  ______ _     _ _______         ||\n"
            ++ "||          |______   |   |  ____ |_____|    |            ||\n"
            ++ "||          |       __|__ |_____| |     |    |            ||\n"                        
            ++ "()========================================================()\n"
            ++ "|| Enemy Stats:                                           ||\n"
            ++ "|| Name: " ++ name ++ replicate (49 - length name) ' ' ++ "||\n"
            ++ "|| HP: " ++ show hp ++ replicate (51 - length (show hp)) ' ' ++ "||\n"
            ++ "|| ATK: " ++ show atk ++ replicate (50 - length (show atk)) ' ' ++ "||\n"
            ++ "|| DEF: " ++ show def ++ replicate (50 - length (show def)) ' ' ++ "||\n"
            ++ "()========================================================()\n"
            ++ "|| Your Stats:                                            ||\n"
            ++ "|| HP: " ++ show hpp ++ replicate (51 - length (show hpp)) ' ' ++ "||\n"
            ++ "|| ATK: " ++ show atkp ++ replicate (50 - length (show atkp)) ' ' ++ "||\n"
            ++ "|| DEF: " ++ show defp ++ replicate (50 - length (show defp)) ' ' ++ "||\n"
            ++ "()========================================================()\n"
            ++ " | What will you do?                                      | \n"
            ++ " | - Attack                                               | \n"
            ++ " | - Use Item                                             | \n"
            ++ " +--------------------------------------------------------+ \n"

invalid_input :: String
invalid_input = "Invalid Input!"
