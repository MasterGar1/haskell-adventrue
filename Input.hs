{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Input where
import Objects

-- Basic states for the game state machine
data State = Start | Help | Explore | Fight
    deriving (Eq, Show, Read)

-- Type of the things kept after each game loop
type GameData = (State, Scene, History, Time)

-- Input Parser
parse_input :: String -> State -> Scene -> History -> Time -> IO GameData
parse_input line st sc@(pl, wd) hs tm
    | line == "quit"= output "Game Exited!" ret
    | line == "log" = output (foldr (\el res ->el ++ " " ++ res) "" hs) ret
    | st == Explore =
        case line of
            "help"      -> help (Help, sc, hlog, time)
            "left"      -> move_player (-1, 0) ret_log
            "right"     -> move_player (1, 0) ret_log
            "up"        -> move_player (0, -1) ret_log
            "down"      -> move_player (0, 1) ret_log
            "inventory" -> show_inventory ret_log
            "skills"    -> show_skills ret_log
            "stats"     -> show_stats ret_log
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
            "combat"   -> help_combat (Help, sc, hlog, time)
            "explore"  -> help_explore (Help, sc, hlog, time)
            "skills"   -> help_skills (Help, sc, hlog, time)
            "items"    -> help_items (Help, sc, hlog, time)
            "exit"     -> do
                           redraw_room sc
                           return (Explore, sc, hs, time)
            _          -> do
                           r <- help (Help, sc, hs, time)
                           output invalid_input r
    | st == Start    =
        case line of
            "start" -> do
                        r <- output "Game Started!" (Explore, sc, hs, time)
                        redraw_room sc
                        return r
            "help"  -> help (Help, sc, hlog, time)
            _       -> output invalid_input (Start, sc, hs, time)
    | otherwise     = error "Invalid State"
    where
        time    = tm + 1
        ret     = (st, sc, hs, time)
        ret_log = (st, sc, hlog, time)
        hlog    = line : hs

-- Checks if an event is encountered while player is exploring
event_handler :: Tile -> Entity -> WorldMap -> History -> Time -> IO GameData
event_handler (E (Enemy _ _ _ _ nm)) pl wd hlog time = do
    putStrLn $ "You encountered an enemy: " ++ nm ++ "!"
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

-- Player actions: movement, attack choice, item choice
move_player :: Coords -> GameData -> IO GameData
move_player dir (_, (pl, wd), hs, tm) = do
                         let player = move dir pl wd
                         if position pl == position player
                            then putStrLn "You can't go there!"
                            else putStr ""
                         event_handler (current_tile (position player) wd) player wd hs tm

choose_attack :: [Skill] -> IO Skill
choose_attack sks = do
                        putStrLn $ print_skills sks
                        putStr "> "
                        line <- getLine
                        if all is_digit line && not (null line)
                            then do
                                let index = read line :: Int
                                if index < 1 || index > length sks
                                    then do
                                        putStrLn invalid_input
                                        choose_attack sks
                                    else
                                        return $ sks !! (index - 1)
                            else do
                                putStrLn invalid_input
                                choose_attack sks

choose_item :: Inventory -> IO (Maybe Int)
choose_item items = do
                    let cons = get_consumable items
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

-- Enemy actions and death handling
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

enemy_attack :: Entity -> Entity -> Time -> IO (Entity, Entity)
enemy_attack pl en@(Enemy _sssssss _ _ sk _) seed = do
    let chosen_skill = pick_random sk $ fromInteger seed
    putStrLn $ "The enemy used " ++ title chosen_skill ++ "!"
    case chosen_skill of
        (Offensive _ skill _) -> do
            let new_player = revert_passive $ skill en (apply_passive pl)
            return (new_player, en)
        (Defensive _ skill _) -> do
            let new_enemy = skill en en
            return (pl, new_enemy)
enemy_attack _ _ _ = error "Not an enemy!"

-- Ability and item usage
use_skill :: Skill -> Entity -> Entity -> WorldMap -> [String] -> Time -> IO GameData
use_skill (Offensive _ skill _) pl en wd hlog time = do
        let pass = gather_passive True (inventory pl)
        let new_enemy = pass $ skill pl en
        enemy_application new_enemy pl wd hlog time

use_skill (Defensive _ skill _) pl enemy wd hlog time = do
        let new_player = skill pl pl
        enemy_application enemy new_player wd hlog time

apply_item :: Int -> Entity -> WorldMap -> [String] -> Time -> IO GameData
apply_item idx pl wd hlog time = do
        let itm = inventory pl !! idx
        let player = manip_inventory pl (use_item idx)
        let (E enemy) = current_tile (position player) wd
        if is_offensive itm
            then do
                let new_enemy = itm `effect` enemy
                let world = update_tile (position player) (E new_enemy) wd
                enemy_application new_enemy player world hlog time
            else do
                let new_player = itm `effect` player
                enemy_application enemy new_player wd hlog time



-- Helper Functions
output :: String -> GameData -> IO GameData
output line sc = do
                   putStrLn line
                   return sc

is_digit :: Char -> Bool
is_digit c = c >= '0' && c <= '9'

redraw_room :: Scene -> IO()
redraw_room (pl, wd) = do
                        print_room pl $ get_room (fst $ position pl) wd

to_lower :: String -> String
to_lower str = [ if is_upper x then make_lower x else x | x <- str ]
    where
        is_upper c   = c > 'A' && c < 'Z'
        make_lower c = toEnum (fromEnum c + 32) :: Char

split :: Char -> String -> [String]
split _ [] = [[]]
split ch (x:xs)
    | x == ch   = [] : split ch xs
    | otherwise = rest $ split ch xs
        where 
            rest [] = [[x]]
            rest (y:ys) = (x : y) : ys

concat_right :: String -> String -> String
concat_right base other = concat $ zipWith (\l1 l2 -> l1 ++ l2 ++ "\n") b_spl o_spl
    where
        b_spl = split '\n' base
        o_spl = split '\n' other

elongate_line :: String -> String
elongate_line line = case line of
    "hp"  -> "help"
    "l"   -> "left"
    "r"   -> "right"
    "u"   -> "up"
    "d"   -> "down"
    "inv" -> "inventory"
    "sks" -> "skills"
    "sts" -> "stats"
    "atk" -> "attack"
    "@"   -> "attack"
    "itm" -> "item"
    "bk"  -> "back"
    "ext" -> "exit"
    "qt"  -> "quit"
    "cbt" -> "combat"
    "exp" -> "explore"
    _     -> line

--Interfaces
show_inventory :: GameData -> IO GameData
show_inventory (st, sc@(pl, wd), hs, tm) = do
    putStrLn $ print_inventory (inventory pl)
    if null $ inventory pl
        then do
            redraw_room sc
            return (Explore, (pl, wd), hs, tm)
        else do
        putStrLn "Type the item number to use it or 'back' to return!"
        putStr "> "
        line <- getLine
        if line == "back"
            then do
                redraw_room sc
                return (Explore, (pl, wd), hs, tm)
            else
                if all is_digit line && not (null line)
                    then do
                        let index = read line :: Int
                        if index < 1 || index > length (inventory pl)
                            then do
                                putStrLn invalid_input
                                show_inventory (st, sc, hs, tm)
                            else do
                                let itm = inventory pl !! (index - 1)
                                case itm of
                                    (Consumable {}) -> do
                                            let player = manip_inventory pl (use_item (index - 1))
                                            let new_player = itm `effect` player
                                            putStrLn ("You used " ++ label itm ++ "!")
                                            redraw_room (new_player, wd)
                                            return (st, (new_player, wd), hs, tm)
                                    (Passive {})     -> do
                                            putStrLn $ print_item itm
                                            show_inventory (st, sc, hs, tm)
                    else do
                            putStrLn invalid_input
                            show_inventory (st, sc, hs, tm)

show_skills :: GameData -> IO GameData
show_skills (st, sc@(pl, _), hs, tm) = do
    putStrLn $ print_skills (skills pl)
    putStrLn "Type 'back' to return or skill number to see info!"
    line <- getLine
    if line == "back"
        then do
        redraw_room sc
        return (Explore, sc, hs, tm)
        else if all is_digit line && not (null line)
                then do
                    let index = read line :: Int
                    if index < 1 || index > length (inventory pl)
                        then do
                            putStrLn invalid_input
                            show_skills (st, sc, hs, tm)
                        else do
                            let sk = skills pl !! (index - 1)
                            putStrLn (print_skill_desc sk)
                            show_skills (st, sc, hs, tm)
                else do
                    putStrLn invalid_input
                    show_skills (st, sc, hs, tm)

show_stats :: GameData -> IO GameData
show_stats (st, sc@(pl, _), hs, tm) = do
    print pl
    putStrLn "Type back to return!"
    line <- getLine
    if line == "back"
        then do
            redraw_room sc
            return (Explore, sc, hs, tm)
        else do
            putStrLn invalid_input
            show_stats (st, sc, hs, tm)

print_room :: Entity -> Room -> IO()
print_room pl room = do
                putStrLn $ concat_right rm inf
                where
                    indexed = zip [0..] (map (zip [0..]) room)
                    pos     = snd $ position pl
                    tpl     = E pl
                    rm      = "+-----------------------+\n|                       |\n"
                                ++ concat [ "|   "
                                                ++ concat [ el ++ "   " | (x, obj) <- row,
                                                        let el = if (x, y) == pos
                                                                    then show tpl else show obj]
                                ++ "|\n|                       |\n" | (y, row) <- indexed ]
                                ++ "+-----------------------+\n"
                    inf     =  "-----------------------+\n"
                            ++ "   Movement:           |\n"
                            ++ "    - left / l         |\n"
                            ++ "    - down / d         |\n"
                            ++ "    - right / r        |\n"
                            ++ "    - up / u           |\n"
                            ++ "   Information:        |\n"
                            ++ "    - inventory / inv  |\n"
                            ++ "    - skills / sks     |\n"
                            ++ "    - stats / sts      |\n"
                            ++ "    - help / hp        |\n"
                            ++ "                       |\n"
                            ++ "-----------------------+\n"

start :: String
start = "+-------------------------------------------------------------+\n"
     ++ "| Hello, adventurer!                                          |\n"
     ++ "| The winds of fate have carried you to Eryndor,              |\n"
     ++ "| a land of untold riches and relentless challenges.          |\n"
     ++ "| Scattered across these wild lands are ancient chests,       |\n"
     ++ "| each brimming with mystery and magic.                       |\n"
     ++ "| Some hold treasures beyond your dreams;                     |\n"
     ++ "| others, trials to test your strength and resolve.           |\n"
     ++ "| The question is simple:                                     |\n"
     ++ "| Will you risk it all for glory and the thrill of discovery? |\n"
     ++ "| Your path begins now, adventurer.                           |\n"
     ++ "| Seek the chests, face the beasts,                           |\n"
     ++ "| and prove that you are worthy of the legend.                |\n"
     ++ "|                                                             |\n"
     ++ "| Type 'start' to begin your journey.                         |\n"
     ++ "| Type 'help' to learn more about the gameplay.               |\n"
     ++ "+-------------------------------------------------------------+\n"

help :: GameData -> IO GameData
help gd = do
    putStrLn txt
    return gd
    where
        txt =  "()================================()\n"
            ++ "|| _     _ _______         _____  ||\n"
            ++ "|| |_____| |______ |      |_____] ||\n"
            ++ "|| |     | |______ |_____ |       ||\n"
            ++ "||                                ||\n"
            ++ "()================================()\n"
            ++ " | What do you need help with?    | \n"
            ++ " | - Combat                       | \n"
            ++ " | - Explore                      | \n"
            ++ " | - Skills                       | \n"
            ++ " | - Items                        | \n"
            ++ " | - Exit                         | \n"
            ++ " +--------------------------------+ \n"

help_combat :: GameData -> IO GameData
help_combat gd = do
    putStrLn txt
    putStr "> "
    l <- getLine
    let line = elongate_line l
    if line == "back"
        then help gd
        else do
            r <- output invalid_input gd
            help_combat r
    where txt = "()=================================================()\n"
             ++ "|| _______  _____  _______ ______  _______ _______ ||\n"
             ++ "|| |       |     | |  |  | |_____] |_____|    |    ||\n"
             ++ "|| |_____  |_____| |  |  | |_____] |     |    |    ||\n"
             ++ "||                                                 ||\n"
             ++ "()=================================================()\n"
             ++ " | You can enter combat by encountering an enemy.  | \n"
             ++ " | Doing so, a fight interface will be opened.     | \n"
             ++ " | There, you will be able to see your and the     | \n"
             ++ " | enemy's stats, and choose your actions.         | \n"
             ++ " | There are two types of actions:                 | \n"
             ++ " | - Attack or Atk: Choose a skill to use          | \n"
             ++ " | - Item or Itm: Use a consumable item            | \n"
             ++ " | The fight will continue until one of the        | \n"
             ++ " | combatants is defeated.                         | \n"
             ++ " |                                                 | \n"
             ++ " +-------------------------------------------------+ \n"
             ++ " | Type 'back' to return to the help menu.         | \n"
             ++ " +-------------------------------------------------+ \n"

help_explore :: GameData -> IO GameData
help_explore gd = do
    putStrLn txt
    putStr "> "
    l <- getLine
    let line = elongate_line l
    if line == "back"
        then help gd
        else do
            r <- output invalid_input gd
            help_explore r
    where txt = "()========================================================()\n"
             ++ "|| _______ _     _  _____          _____   ______ _______ ||\n"
             ++ "|| |______  \\___/  |_____] |      |     | |_____/ |______ ||\n"
             ++ "|| |______ _/   \\_ |       |_____ |_____| |    \\_ |______ ||\n"
             ++ "||                                                        ||\n"
             ++ "()========================================================()\n"
             ++ " | You can explore the world by moving your character.    | \n"
             ++ " | You can move in four directions:                       | \n"
             ++ " | - Left or L: Move to the left                          | \n"
             ++ " | - Right or R: Move to the right                        | \n"
             ++ " | - Up or U: Move up                                     | \n"
             ++ " | - Down or D: Move down                                 | \n"
             ++ " | You can also check your inventory, skills and stats.   | \n"
             ++ " | To do so, type 'inventory' / 'inv,                     | \n"
             ++ " | 'skills' / 'sks' or 'stats' / 'sts'.                   | \n"
             ++ " | While exploring you may encounter enemies or chests.   | \n"
             ++ " | Enemies will trigger combat, while chests will give    | \n"
             ++ " | you items or skill books.                              | \n"
             ++ " +--------------------------------------------------------+ \n"
             ++ " | Type 'back' to return to the help menu.                | \n"
             ++ " +--------------------------------------------------------+ \n"

help_skills :: GameData -> IO GameData
help_skills gd = do
    putStrLn txt
    putStr "> "
    l <- getLine
    let line = elongate_line l
    if line == "back"
        then help gd
        else do
            r <- output invalid_input gd
            help_skills r
    where txt = "()========================================================()\n"
             ++ "||      _______ _     _ _____               _______       ||\n"
             ++ "||      |______ |____/    |   |      |      |______       ||\n"
             ++ "||      ______| |    \\_ __|__ |_____ |_____ ______|       ||\n"
             ++ "||                                                        ||\n"
             ++ "()========================================================()\n"
             ++ " | You can check your skills by typing 'skills' or 'sks'. | \n"
             ++ " | There, you will be able to see all your available      | \n"
             ++ " | skills.                                                | \n"
             ++ " | You can see a skill's effect by selecting it.          | \n"
             ++ " | You can use your skills in combat to defeat enemies.   | \n"
             ++ " | Skills are divided into two categories:                | \n"
             ++ " | - Offensive: Skills that deal damage to enemies.       | \n"
             ++ " | - Defensive: Skills that heal or buff your character.  | \n"
             ++ " | Remember that you can only use one skill per turn.     | \n"
             ++ " +--------------------------------------------------------+ \n"
             ++ " | Type 'back' to return to the help menu.                | \n"
             ++ " +--------------------------------------------------------+ \n"

help_items :: GameData -> IO GameData
help_items gd = do
    putStrLn txt
    putStr "> "
    l <- getLine
    let line = elongate_line l
    if line == "back"
        then help gd
        else do
            r <- output invalid_input gd
            help_items r
    where txt = "()=======================================================()\n"
             ++ "||         _____ _______ _______ _______ _______         ||\n"
             ++ "||           |      |    |______ |  |  | |______         ||\n"
             ++ "||         __|__    |    |______ |  |  | ______|         ||\n"
             ++ "||                                                       ||\n"
             ++ "()=======================================================()\n"
             ++ " | You can check your inventory by typing 'inventory'    | \n"
             ++ " | or 'inv'. There, you will be able to see all the      | \n"
             ++ " | items you have collected.                             | \n"
             ++ " | You can use consumable items in combat to heal or     | \n"
             ++ " | buff your character.                                  | \n"
             ++ " | Passive items to permanently increase your stats      | \n"
             ++ " | before each turn.                                     | \n"
             ++ " | Remember that you can only use one item per turn.     | \n"
             ++ " +-------------------------------------------------------+ \n"
             ++ " | Type 'back' to return to the help menu.               | \n"
             ++ " +-------------------------------------------------------+ \n"

combat_screen :: Tile -> Entity -> String
combat_screen (E (Enemy hp atk def _ nm)) (Player hpp atkp defp _ _ _) =
               "()========================================================()\n"
            ++ "||          _______ _____  ______ _     _ _______         ||\n"
            ++ "||          |______   |   |  ____ |_____|    |            ||\n"
            ++ "||          |       __|__ |_____| |     |    |            ||\n"
            ++ "()========================================================()\n"
            ++ "|| Enemy Stats:              || Your Stats:               ||\n"
            ++ "|| Name: " ++ nm ++ replicate (20 - length nm) ' ' ++ "||"
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
combat_screen _ _ = "Invalid arguments!"


death :: String
death = "+----------------------------------------------------------------+\n"
     ++ "| __   __  _____  _     _      ______  _____ _______ ______    / |\n"
     ++ "|   \\_/   |     | |     |      |     \\   |   |______ |     \\  /  |\n"
     ++ "|    |    |_____| |_____|      |_____/ __|__ |______ |_____/ .  |\n"
     ++ "+----------------------------------------------------------------+\n"


invalid_input :: String
invalid_input = "Invalid Input!"
