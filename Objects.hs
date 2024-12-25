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
type Effect = Multiplier -> Entity -> Hit -- Effects modify entities based on a multiplier
type Ability = Entity -> Hit              -- Abilities define specific entity actions
type History = [String]                   -- Log of past events or actions

-- Definition of skills as offensive or defensive abilities
data Skill = Offensive { title :: String, func :: Ability }
           | Defensive { title :: String, func :: Ability }

-- The main game entities: enemies and the player
data Entity = Enemy  { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], name :: String }
            | Player { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], inventory :: Inventory, position :: Position }

-- Items the player uses
data Item = Consumeable { label :: String, charges :: Int, effect :: Hit, is_offensive :: Bool }
          | Passive     { label :: String, effect :: Hit, is_offensive :: Bool }

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
    show (Offensive t _) = "Attack Skill: "  ++ t
    show (Defensive t _) = "Defence Skill: " ++ t

instance Show Item where
    show :: Item -> String
    show (Consumeable l c eff _) = "Item: " ++ show l
                              ++ " | Charges: " ++ show c
    show (Passive l eff _)       = "Item: " ++ show l


instance Show Entity where
    show :: Entity -> String
    show (Enemy h a d s n)    = "Name: " ++ n ++ ['\n']
                             ++ "Health: " ++ show h
                             ++ " | Attack: " ++ show a
                             ++ " | Defence: " ++ show d ++ ['\n']
                             ++ "Skills: " ++ show (map title s)
    show (Player h a d s inv _) = "Health: " ++ show h
                             ++ " | Attack: " ++ show a
                             ++ " | Defence: " ++ show d ++ ['\n']
                             ++ "Inventory: " ++ show inv ++ ['\n']
                             ++ "Skills: " ++ show (map title s)

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
    (Passive l1 _ _)       == (Passive l2 _ _)       = l1 == l2
    _                      == _                      = False

-- Entity updater
update :: Health -> Attack -> Defence -> Hit
update hmod amod dmod (Player hp atk def sk inv cord) = Player hp' atk' def' sk inv cord
    where
        hp' = hp + hmod
        atk' = atk + amod
        def' = def + dmod
update hmod amod dmod (Enemy hp atk def sk nm)        = Enemy  hp' atk' def' sk nm
    where
            hp' = hp + hmod
            atk' = atk + amod
            def' = def + dmod

-- Base Skills
heal :: Effect
heal mult user = update amount 0 0
    where
        amount = floor $ max 0 $ mult * fromIntegral (attack user)


deal_damage :: Effect
deal_damage mult user = update amount 0 0
    where
        amount = floor $ min 0 $ mult * fromIntegral (-attack user)

-- Print functions
print_skills :: [Skill] -> String
print_skills skills = "Skills:\n" ++
    unlines (zipWith (\x y -> show x ++ ". " ++ show y) [1..] skills)

print_inventory :: Inventory -> String
print_inventory inv = "Inventory:\n" ++
    unlines (zipWith (\x y -> show x ++ ". " ++ show y) [1..] inv)

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
