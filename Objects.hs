{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Objects where

-- Some constants
room_size :: (Int, Int)
room_size = (5, 5)

map_size :: (Int, Int)
map_size = (10, 10)

-- Types for various properties of the game
type Multiplier = Float                   -- Used for scaling effects
type Time = Integer                       -- Time type for random number generation
type Damage = Int                         -- Represents attack damage
type Health = Damage                      -- Alias for health, as it relates to damage
type Attack = Damage                      -- Alias for attack stats
type Defence = Damage                     -- Alias for defense stats
type Inventory = [Item]                   -- List of items held by an entity
type Coords = (Int, Int)                  -- A pair representing coordinates (x, y)
type Position = (Coords, Coords)          -- Global and local coordinates
type Scene = (Entity, WorldMap)           -- A scene with one entity and the world map
type Hit = Entity -> Entity               -- A transformation function for entities
type Ability = Entity -> Hit              -- Abilities define specific entity actions
type Effect = Multiplier -> Entity -> Hit -- Effects modify entities based on a multiplier
type History = [String]                   -- Log of past events or actions

-- Definition of skills as offensive or defensive abilities
data Skill = Offensive { title :: String, func :: Ability, desc :: String }
           | Defensive { title :: String, func :: Ability, desc :: String }

-- The main game entities: enemies and the player
data Entity = Enemy  { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], name :: String }
            | Player { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], inventory :: Inventory, position :: Position }

-- Items the player uses
data Item = Consumeable { label :: String, charges :: Int, effect :: Hit, is_offensive :: Bool }
          | Passive     { label :: String, effect :: Hit, is_offensive :: Bool, info :: String }

-- Objects present on the map
data Object = None | Wall | Chest { loot :: Item }

-- Tiles are either objects or entities
data Tile = O Object | E Entity

-- A room is a grid of tiles
type Room = [[Tile]]

-- A world map is a grid of rooms
type WorldMap = [[Room]]

-- Redefine classes to provide custom string representations
instance Show Skill where
    show :: Skill -> String
    show (Offensive t _ _) = "Attack Skill: "  ++ t
    show (Defensive t _ _) = "Defence Skill: " ++ t

instance Show Item where
    show :: Item -> String
    show (Consumeable l c _ _) = l ++ " | Charges: " ++ show c
    show (Passive l _ _ _)       = show l


instance Show Entity where
    show :: Entity -> String
    show (Enemy h a d s n)    = "Name: " ++ n ++ ['\n']
                             ++ "Health: " ++ show h
                             ++ " | Attack: " ++ show a
                             ++ " | Defence: " ++ show d ++ ['\n']
                             ++ "Skills: " ++ show (map title s)
    show (Player h a d _ _ _) = "+--------------------+\n"
                             ++ "| Your Stats:        |\n"
                             ++ "+--------------------+\n"
                             ++ "| Health: " ++ show h ++ replicate (10 - length (show h)) ' ' ++ " |\n"
                             ++ "| Attack: " ++ show a ++ replicate (10 - length (show a)) ' ' ++" |\n"
                             ++ "| Defence: " ++ show d ++ replicate (9 - length (show d)) ' ' ++ " |\n"
                             ++ "+--------------------+"

instance Show Tile where
    show :: Tile -> String
    show (E (Enemy {}))  = "E"
    show (E (Player {})) = "P"
    show (O (Chest {}))  = "C"
    show (O None)        = "."
    show (O Wall)        = "#"

instance Eq Item where
    (==) :: Item -> Item -> Bool
    (Consumeable l1 _ _ _) == (Consumeable l2 _ _ _) = l1 == l2
    (Passive l1 _ _ _)       == (Passive l2 _ _ _)   = l1 == l2
    _                      == _                      = False

instance Eq Skill where
    (==) :: Skill -> Skill -> Bool
    (Offensive t1 _ _) == (Offensive t2 _ _) = t1 == t2
    (Defensive t1 _ _) == (Defensive t2 _ _) = t1 == t2
    _                == _                = False

-- Entity updater
update :: Health -> Attack -> Defence -> [Skill] -> Entity -> Entity
update hmod amod dmod nsk (Enemy hp atk def sk nm) =
        Enemy  (hp + hmod') (atk + amod) def' (gain_skill nsk sk) nm
        where
            hmod' = defence_bonus hmod def'
            def'  = def + dmod

update hmod amod dmod nsk (Player hp atk def sk inv cord) =
        Player (hp + hmod') (atk + amod) def' (gain_skill nsk sk) inv cord
        where
            hmod' = defence_bonus hmod def'
            def'  = def + dmod

defence_bonus :: Health -> Defence -> Health
defence_bonus dmg def
    | dmg < 0 = min 0 $ dmg + def
    | otherwise = dmg


apply_passive :: Entity -> Entity
apply_passive pl = gather_passive False (inventory pl) pl

revert_passive :: Entity -> Entity
revert_passive pl@(Player hp atk def sk inv cords) =
    Player (hp - dhp) (atk - datk) (def - ddef) sk inv cords
    where
        Player hp' atk' def' _ _ _ = gather_passive False inv pl
        dhp  = hp' - hp
        datk = atk' - atk
        ddef = def' - def

gain_skill :: [Skill] -> [Skill] -> [Skill]
gain_skill [] skills = skills
gain_skill (s:ss) skills = if s `elem` skills
                            then gain_skill ss skills
                            else gain_skill ss $ s : skills


-- Base Skills
heal :: Effect
heal mult user = update amount 0 0 []
    where
        amount = floor $ max 0 $ mult * fromIntegral (attack user)


deal_damage :: Effect
deal_damage mult user = update amount 0 0 []
    where
        amount = floor $ min 0 $ mult * fromIntegral (-attack user)
        
-- Skill Composition
(...) :: Ability -> Ability -> Entity -> Hit
(sk1 ... sk2) user = sk1 user . sk2 user

-- Print functions
print_skills :: [Skill] -> String
print_skills skills = "+" ++ replicate (spaces + 4) '-' ++ "+\n"
    ++ "| Skills:" ++ replicate (spaces - 5) ' ' ++ " |\n"
    ++ "+" ++ replicate (spaces + 4) '-' ++ "+\n"
    ++ unlines (zipWith (\x y -> "| " ++ show x ++ ". " ++ show y 
                ++ replicate (spaces - length (show y) - length (show x)) ' ' ++ " |") [1..] skills)
    ++ "+" ++ replicate (spaces + 4) '-' ++ "+"
    where
        spaces = maximum (map (length . show) skills) + maximum (map (length . show) [1..(length skills)])

print_inventory :: Inventory -> String
print_inventory [] = "Inventory is empty."
print_inventory inv = "+" ++ replicate (spaces + 4) '-' ++ "+\n"
    ++ "| Inventory:" ++ replicate (spaces - 8) ' ' ++ " |\n"
    ++ "+" ++ replicate (spaces + 4) '-' ++ "+\n"
    ++ unlines (zipWith (\x y -> "| " ++ show x ++ ". " ++ show y 
                        ++ replicate (spaces - length (show y) - length (show x)) ' ' ++ " |") [1..] inv) 
    ++ "+" ++ replicate (spaces + 4) '-' ++ "+"
    where 
        spaces = maximum (map (length . show) inv) + maximum (map (length . show) [1..(length inv)])

print_item :: Item -> String
print_item (Passive name _ _ desc) = "+" ++ replicate (spaces + 2) '-' ++ "+\n"
                                  ++ "| Item: " ++ name ++ replicate (spaces - 6 - length name) ' ' ++ " |\n"
                                  ++ "| " ++ desc ++ replicate (spaces - length desc) ' ' ++ " |\n"
                                  ++ "+" ++ replicate (spaces + 2) '-' ++ "+\n"
    where 
        spaces = max (length desc) (length name + 6)
print_item itm                     = show itm


print_skill_desc :: Skill -> String
print_skill_desc skill = "+" ++ replicate (spaces + 2) '-' ++ "+\n"
                                  ++ "| Skill: " ++ title skill ++ replicate (spaces - 7 - length (title skill)) ' ' ++ " |\n"
                                  ++ "| " ++ desc skill ++ replicate (spaces - length (desc skill)) ' ' ++ " |\n"
                                  ++ "+" ++ replicate (spaces + 2) '-' ++ "+\n"
    where 
        spaces = max (length $ desc skill) (7 + length (title skill))

-- Checks if an entity is dead
is_dead :: Entity -> Bool
is_dead (Player hp _ _ _ _ _) = hp <= 0
is_dead (Enemy hp _ _ _ _)    = hp <= 0

-- Inventory Operations
manip_inventory :: Entity -> (Inventory -> Inventory) -> Entity
manip_inventory (Player hp atk def sk inv cord) f = Player hp atk def sk (f inv) cord

is_passive :: Item -> Bool
is_passive (Passive {}) = True
is_passive _            = False

get_passive :: Inventory -> Inventory
get_passive = filter is_passive

get_consumeable :: Inventory -> Inventory
get_consumeable = filter $ not . is_passive

gather_passive :: Bool -> Inventory -> Hit
gather_passive is_off inv = foldr (.) id selected
    where selected = [ effect itm | itm <- inv, is_passive itm && is_offensive itm == is_off ]

gain_item :: Item -> Inventory -> Inventory
gain_item itm inv = inv ++ [itm]

clear_used :: Inventory -> Inventory
clear_used = filter $ not . is_used
    where
        is_used (Consumeable _ charges _ _) = charges == 0
        is_used _                           = False

use_item :: Int -> Inventory -> Inventory
use_item idx inv = clear_used $ take idx inv ++ [after itm] ++ drop (idx + 1) inv
    where
        itm   = inv !! idx
        after (Consumeable l c e o) = Consumeable l (c - 1) e o
        after itm                 = itm

find_useable_index :: Int -> Inventory -> Int
find_useable_index index items
    | index >= length filtered = -1
    | otherwise                = fst (filtered !! index)
    where
        filtered = filter (not . is_passive . snd) (zip [0..] items)

-- Random number generation
random_index :: Int -> Time -> Int
random_index len seed = if len <= 1
                            then 0
                            else fromIntegral $ rand seed `mod` toInteger len

pick_random :: [a] -> Int -> a
pick_random lst seed = lst !! idx
    where idx = random_index (length lst) $ toInteger seed

-- Using the formula in this article: 
-- https://en.wikipedia.org/wiki/Random_number_generation
rand :: Time -> Time
rand seed = (1103515245 * seed + 12345) `mod` 2147483647

random_range :: Time -> Integer -> Integer -> Time
random_range seed a b = if b - a <= 0 then 0 else (rand seed `mod` (b - a)) + a

-- If pos is outside world, it is clamped inside to the nearest room
get_room :: Coords -> WorldMap -> Room
get_room pos@(x, y) world = world !! clamp_y !! clamp_x
                            where
                                clamp_x = max 0 $ min x (fst room_size - 1)
                                clamp_y = max 0 $ min y (snd room_size - 1)

-- If pos is outside, it wraps around the room
get_tile :: Coords -> Room -> Tile
get_tile pos@(x, y) room = room !! clamp_y !! clamp_x
                            where
                                clamp_x = x `mod` fst room_size
                                clamp_y = y `mod` snd room_size

current_tile :: Position -> WorldMap -> Tile
current_tile (global, local) wd = get_tile local $ get_room global wd

update_tile :: Position -> Tile -> WorldMap -> WorldMap
update_tile ((g1, g2), (l1, l2)) what wd = take g2 wd ++
                                    [take g1 (wd !! g2) ++
                                        [take l2 (wd !! g2 !! g1) ++
                                            [take l1 (wd !! g2 !! g1 !! l2)
                                                ++ [what]
                                            ++ drop (l1 + 1) (wd !! g2 !! g1 !! l2)]
                                        ++ drop (l2 + 1) (wd !! g2 !! g1)]
                                    ++ drop (g1 + 1) (wd !! g2)]
                                ++ drop (g2 + 1) wd

is_inside :: Coords -> Coords -> Bool
is_inside (x, y) (a, b) = x < a && y < b && x >= 0 && y >=0

-- Movement
direct :: Coords -> Coords -> Coords
direct (x, y) (a, b) = (x + a, y + b)

-- Dir is one of: down(0, 1) left(1, 0) up(0, -1) right(-1, 0)
move :: Coords -> Entity -> WorldMap -> Entity
move dir pl@(Player hp atk def sk inv (global, local)) wd
    | is_inside new_local room_size
    && can_step new_local global wd =
        Player hp atk def sk inv (global, new_local)
    | is_inside new_global map_size
    && not (is_inside new_local room_size)
    && can_step (clamp new_local) new_global wd =
        Player hp atk def sk inv (new_global, clamp new_local)
    | otherwise = pl
    where
        new_local  = local `direct` dir
        new_global = global `direct` dir
        clamp (x, y)  = (x `mod` fst room_size, y `mod` snd room_size)

can_step :: Coords -> Coords -> WorldMap -> Bool
can_step local global wd = not $ is_wall (get_tile local $ get_room global wd)
    where
        is_wall (O Wall) = True
        is_wall _        = False
