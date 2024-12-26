{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where
import Objects

data State = Start | Help | Explore | Fight
    deriving (Eq, Show, Read)

type GameData = (State, Scene, History, Time)

elongate_line :: String -> String
elongate_line line = case line of
    "hp" -> "help"
    "l" -> "left"
    "r" -> "right"
    "u" -> "up"
    "d" -> "down"
    "inv" -> "inventory"
    "sks" -> "skills"
    "sts" -> "stats"
    "atk" -> "attack"
    "itm" -> "item"
    _ -> line

parse_input :: String -> State -> Scene -> History -> Time -> IO GameData
parse_input l st sc@(pl, wd) hs tm
    | line == "quit"= output "Game Exited!" ret
    | line == "log" = output (foldr (\el res ->el ++ " " ++ res) "" hs) ret
    | st == Explore =
        case line of
            "help"      -> output help (Help, sc, hlog, time)
            "left"      -> move_player (-1, 0) ret_log
            "right"     -> move_player (1, 0) ret_log
            "up"        -> move_player (0, -1) ret_log
            "down"      -> move_player (0, 1) ret_log
            "inventory" -> show_inventory ret_log
            "skills"    -> show_skills ret_log
            "stats"    -> show_stats ret_log
            _           -> output invalid_input ret
    | st == Fight   =
        case line of
            "attack" -> do
                         chosen_skill <- choose_attack (skills pl)
                         let (E enemy) = current_tile (position pl) wd
                         putStrLn $ "You used " ++ title chosen_skill ++ "!"
                         use_skill chosen_skill pl enemy wd hs tm
            "item"   -> do
                         itm_index <- choose_item (inventory pl)
                         case itm_index of
                             Just idx -> do
                                          putStrLn ("You used " ++ label (inventory pl !! idx) ++ "!")
                                          apply_item idx pl wd hs tm
                             Nothing   -> do
                                           r <- output "Your inventory is empty!" ret
                                           output (combat_screen (current_tile (position pl) wd) pl) r
            _        -> do
                         r <- output invalid_input ret_log
                         output (combat_screen (current_tile (position pl) wd) pl) r
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
        line    = elongate_line l
        time    = tm + 1
        ret     = (st, sc, hs, time)
        ret_log = (st, sc, hlog, time)
        hlog    = line : hs


event_handler :: Tile -> Entity -> WorldMap -> History -> Time -> IO GameData
event_handler (E (Enemy hp atk def sk name)) pl wd hlog time = do
    putStrLn $ "You encountered an enemy: " ++ name ++ "!"
    output (combat_screen (current_tile (position pl) wd) pl) (Fight, (pl, wd), hlog, time)

event_handler (O (Chest item)) pl wd hlog time = do
    let new_player = manip_inventory pl (gain_item item)
    let new_world = update_tile (position new_player) (O None) wd
    r <- output ("You found a chest with " ++ label item ++ "!") (Explore, (new_player, new_world), hlog, time)
    redraw_room (new_player, new_world)
    return r

event_handler _ pl wd hlog time = do
    redraw_room (pl, wd)
    return (Explore, (pl, wd), hlog, time)

output :: String -> GameData -> IO GameData
output line sc = do
                   putStrLn line
                   return sc

choose_item :: Inventory -> IO (Maybe Int)
choose_item items = do
                    let cons = get_consumeable items
                    if null cons
                        then do
                            return Nothing
                        else do
                            putStrLn $ print_inventory cons
                            putStr "> "
                            line <- getLine
                            if all is_digit line && not (null line)
                                then do
                                    let index = read line :: Int
                                    if index < 1 || index > length cons
                                        then do
                                            putStrLn invalid_input
                                            choose_item items
                                        else return $ Just $ find_useable_index (index - 1) items
                                else do
                                    putStrLn invalid_input
                                    choose_item items
                where is_digit c = c >= '0' && c <= '9'

choose_attack :: [Skill] -> IO Skill
choose_attack skills = do
                        putStrLn $ print_skills skills
                        putStr "> "
                        line <- getLine
                        if all is_digit line && not (null line)
                            then do
                                let index = read line :: Int
                                if index < 1 || index > length skills
                                    then do
                                        putStrLn invalid_input
                                        choose_attack skills
                                    else
                                        return $ skills !! (index - 1)
                            else do
                                putStrLn invalid_input
                                choose_attack skills
                        where is_digit c = c >= '0' && c <= '9'

enemy_application :: Entity -> Entity -> WorldMap -> [String] -> Time -> IO GameData
enemy_application en pl wd hlog tm = do
    if health en <= 0
        then do
            let new_world = update_tile (position pl) (O None) wd
            r <- output "You defeated the enemy!" (Explore, (pl, new_world), hlog, tm)
            redraw_room (pl, new_world)
            return r
        else do
            (player, new_enemy) <- enemy_attack pl en tm
            let world = update_tile (position player) (E new_enemy) wd
            output (combat_screen (current_tile (position player) world) player) (Fight, (player, world), hlog, tm)

use_skill :: Skill -> Entity -> Entity -> WorldMap -> [String] -> Time -> IO GameData
use_skill (Offensive name skill) pl en wd hlog time = do
        let pass = gather_passive True (inventory pl)
        let new_enemy = pass $ skill pl en
        enemy_application new_enemy pl wd hlog time

use_skill (Defensive name skill) pl enemy wd hlog time = do
        let new_player = skill pl pl
        (player, new_enemy) <- enemy_attack new_player enemy time
        output (combat_screen (current_tile (position player) wd) player) (Fight, (player, wd), hlog, time)

apply_item :: Int -> Entity -> WorldMap -> [String] -> Time -> IO GameData
apply_item idx pl wd hlog time = do
        let itm = inventory pl !! idx
        let player = manip_inventory pl (use_item idx)
        let (E enemy) = current_tile (position player) wd
        if is_offensive itm
            then do
                let new_enemy = itm `effect` enemy
                let world = update_tile (position player) (E new_enemy) wd
                return (Fight, (player, world), hlog, time)
            else do
                let new_player = itm `effect` player
                return (Fight, (new_player, wd), hlog, time)

enemy_attack :: Entity -> Entity -> Time -> IO (Entity, Entity)
enemy_attack pl en@(Enemy hp atk def sk _) seed = do
    let idx = random_index (length sk) seed
    let chosen_skill = pick_random sk $ fromInteger seed
    putStrLn $ "The enemy used " ++ title chosen_skill ++ "!"
    case chosen_skill of
        (Offensive name skill) -> do
            let new_player = revert_passive $ skill en (apply_passive pl)
            return (new_player, en)
        (Defensive name skill) -> do
            let new_enemy = skill en en
            return (pl, new_enemy)

move_player :: Coords -> GameData -> IO GameData
move_player dir (st, sc@(pl, wd), hs, tm) = do
                         let player = move dir pl wd
                         let state = parse_event $ current_tile (position player) wd
                         if position pl == position player
                            then putStrLn "You can't go there!"
                            else putStr ""
                         event_handler (current_tile (position player) wd) player wd hs tm
                where 
                    parse_event (E (Enemy {})) = Fight
                    parse_event _              = Explore

show_inventory :: GameData -> IO GameData
show_inventory (st, sc@(pl, wd), hs, tm) = do
                    putStrLn $ print_inventory (inventory pl)
                    return (Explore, (pl, wd), hs, tm)

show_skills :: GameData -> IO GameData
show_skills (st, sc@(pl, wd), hs, tm) = do
                    putStrLn $ print_skills (skills pl)
                    return (Explore, (pl, wd), hs, tm)

show_stats :: GameData -> IO GameData
show_stats (st, sc@(pl, wd), hs, tm) = do
                    print pl
                    return (Explore, (pl, wd), hs, tm)

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
            ++ "|| Enemy Stats:              || Your Stats:               ||\n"
            ++ "|| Name: " ++ name ++ replicate (20 - length name) ' ' ++ "||"
            ++ " HP: " ++ show hpp ++ replicate (22 - length (show hpp)) ' ' ++ "||\n"
            ++ "|| HP: " ++ show hp ++ replicate (22 - length (show hp)) ' ' ++ "||"
            ++ " ATK: " ++ show atkp ++ replicate (21 - length (show atkp)) ' ' ++ "||\n"
            ++ "|| ATK: " ++ show atk ++ replicate (21 - length (show atk)) ' ' ++ "||"
            ++ " DEF: " ++ show defp ++ replicate (21 - length (show defp)) ' ' ++ "||\n"
            ++ "|| DEF: " ++ show def ++ replicate (21 - length (show def)) ' ' ++ "||"
            ++ replicate 27 ' ' ++ "||\n"
            ++ "()========================================================()\n"
            ++ " | What will you do?                                      | \n"
            ++ " | - Attack                                               | \n"
            ++ " | - Item                                                 | \n"
            ++ " +--------------------------------------------------------+ \n"

invalid_input :: String
invalid_input = "Invalid Input!"
