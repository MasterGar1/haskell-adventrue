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
data Item = Consumeable { label :: String, charges :: Int, effect :: Hit }
          | Passive     { label :: String, effect :: Hit }

-- Objects present on the map
data Object = None | Wall | Chest { loot :: Inventory }

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
    show (Consumeable l c eff) = "Item: " ++ show l
                              ++ " | Charges: " ++ show c
    show (Passive l eff)       = "Item: " ++ show l


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

-- Inventory Operations
get_passive :: Inventory -> Inventory
get_passive = filter is_passive
    where
        is_passive (Passive _ _) = True
        is_passive _             = False

get_consumeable :: Inventory -> Inventory
get_consumeable = filter is_consumable
    where
        is_consumable (Consumeable {}) = True
        is_consumable _                = False

gain_item :: Item -> Inventory -> Inventory
gain_item itm inv = inv ++ [itm]

clear_used :: Inventory -> Inventory
clear_used = filter $ not . is_used
    where
        is_used (Consumeable _ charges _) = charges == 0
        is_used _                         = False

use_item :: Int -> Inventory -> Inventory
use_item idx inv = clear_used $ take idx inv ++ [after itm] ++ drop (idx+1) inv
    where
        itm   = inv !! idx
        after (Consumeable l c e) = Consumeable l (c - 1) e
        after (Passive l e)       = Passive     l e

random_skill :: [Skill] -> Skill
random_skill sk = sk !! fromIntegral (random_range 0 $ toInteger (length sk))

-- Using the formula in this article: 
-- https://en.wikipedia.org/wiki/Random_number_generation
rand :: Time -> Time
rand seed = (1103515245 * seed + 12345) `mod` 2147483647

random_range :: Integer -> Integer -> Time
random_range a b = (rand 3232 `mod` (b - a)) + a

-- Map
{-
    To make a simple bijection seed_? : N^2 -> N,
    We can take inspiration from the triangle numbers' formula:
    S(k) = k * (k+1) / 2
    If we plug k = a + b, then:
    seed_?(a, b) = (a + b) * (a + b + 1) / 2
    But then seed_?(a, b) = seed_?(b, a) ~> We will use either a or b as offset
    => seed_?(a, b) = (a + b) * (a + b + 1) / 2 + a, which is a bijection,
    thus each pair (a, b) has a single mapping in N
    Note: This does not generate truly random rooms, as they can be predicted
-}
generate_entity :: Coords -> Coords -> Tile
generate_entity (l1, l2) (g1, g2)
    | randomness < 2  = O $ Chest []
    | randomness < 10 = O Wall
    | randomness < 15 = E $ Enemy 100 0 0 [] "Imp"
    | otherwise       = O None
    where
        randomness  = seed `mod` 100
        seed        = (seed_global + seed_local) * (seed_global + seed_local + 1) `div` 2 + seed_local
        seed_global = (g1 + g2) * (g1 + g2 + 1) `div` 2 + g2
        seed_local  = (l1 + l2) * (l1 + l2 + 1) `div` 2 + l1


generate_room :: Coords -> Room
generate_room global_coords =
    [ [ generate_entity global_coords (a, b) | b <- [0..snd room_size - 1] ]
                            | a <- [0..fst room_size - 1] ]

generate_map :: Coords -> WorldMap
generate_map (x, y) =
    [ [ generate_room (a, b) | b <- [0..x-1] ] | a <- [0..y-1] ]

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

-- >>>  generate_map (2, 2)
-- [[[[C,#,.,.,#],[C,E,.,.,.],[#,.,.,.,.],[.,.,.,C,.],[.,.,.,.,.]],[[#,#,.,.,.],[#,.,.,#,E],[E,.,.,.,.],[.,.,.,.,.],[.,.,C,.,.]]],[[[#,E,.,.,.],[#,.,.,.,.],[.,.,#,E,.],[.,.,.,.,.],[.,.,.,.,#]],[[E,.,.,.,.],[.,.,.,.,.],[.,.,.,.,.],[.,.,.,E,.],[#,E,.,.,.]]]]
