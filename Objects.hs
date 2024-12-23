{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Objects where

-- Some constants
room_size :: (Int, Int)
room_size = (5, 5)

map_size :: (Int, Int)
map_size = (10, 10)

-- Some types
type Multiplier = Float
type Time = Integer
type Damage = Int
type Health = Damage
type Attack = Damage
type Defence = Damage
type Inventory = [Item]
type Coords = (Int, Int)
type Position = (Coords, Coords)

type Hit = Entity -> Entity
type Effect = Multiplier -> Entity -> Hit
type Ability = Entity -> Hit

-- Useful Data types
data Skill = Offensive { title :: String, func :: Ability } 
           | Defensive { title :: String, func :: Ability }

data Entity = Enemy  { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], name :: String }
            | Player { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], inventory :: Inventory, position :: Position }

data Item = Consumeable { label :: String, charges :: Int, effect :: Hit }
          | Passive     { label :: String, effect :: Hit }

data Object = None | Wall | Chest { loot :: Inventory }

data Tile = O Object | E Entity

type Room = [[Tile]]

type WorldMap = [[Room]]

-- Redefine classes
instance Show Skill where
    show :: Skill -> String
    show (Offensive t _) = "Attack Skill: "  ++ t
    show (Defensive t _) = "Defence Skill: " ++ t

instance Show Item where
    show :: Item -> String
    show (Consumeable l c eff) = "Item: " ++ show l
                              ++ " | Charges: " ++ show c
    show (Passive l eff)       = "Item: " ++ show l

instance Eq Item where
    (==) :: Item -> Item -> Bool
    (Consumeable l1 _ _) == (Consumeable l2 _ _) = l1 == l2
    (Passive l1 _)       == (Passive l2 _)       = l1 == l2
    (Consumeable {})     == (Passive _ _)        = False

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



-- Entity updaters
update :: Health -> Attack -> Defence -> Hit
update hmod amod dmod (Player hp atk def sk inv cord) = Player hp atk def sk inv cord
update hmod amod dmod (Enemy hp atk def sk nm)        = Enemy  hp atk def sk nm

-- Base Skills
heal :: Effect
heal mult user = update amount 0 0
    where
        amount = floor $ min 0 $ mult * fromIntegral (-attack user)


deal_damage :: Effect
deal_damage mult user = update amount 0 0
    where
        amount = floor $ max 0 $ mult * fromIntegral (attack user)

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

remove_item :: Item -> Inventory -> Inventory
remove_item _ [] = []
remove_item itm (x:xs)
    | itm /= x = x : remove_item itm xs
    | itm == x = xs

use_item :: Int -> Inventory -> Inventory
use_item idx inv = clear_used $ take idx inv ++ [after itm] ++ drop (idx+1) inv
    where
        itm   = inv !! idx
        after (Consumeable l c e) = Consumeable l (c - 1) e
        after (Passive l e)       = Passive     l e

random_skill :: [Skill] -> Skill
random_skill sk = sk !! fromIntegral (random_range 0 $ toInteger (length sk))

-- Using the formula in this article: https://en.wikipedia.org/wiki/Random_number_generation
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
    | randomness < 15 = E $ Enemy 0 0 0 [] "Imp"
    | otherwise       = O None
    where
        randomness  = seed `mod` 100
        seed        = (seed_global + seed_local) * (seed_global + seed_local + 1) `div` 2 + seed_local
        seed_global = (g1 + g2) * (g1 + g2 + 1) `div` 2 + g2
        seed_local  = (l1 + l2) * (l1 + l2 + 1) `div` 2 + l1


generate_room :: Coords -> Room
generate_room global_coords =
    [ [ generate_entity global_coords (a, b) | b <- [0..snd room_size] ]
                            | a <- [0..fst room_size] ]

generate_map :: Coords -> WorldMap
generate_map (x, y) =
    [ [ generate_room (a, b) | b <- [0..x] ] | a <- [0..y] ]

print_room :: Room -> IO()
print_room []     = do putChar '\n'
print_room (x:xs) = do
                putStrLn $ foldr
                            ((\el res -> el ++ "   " ++ res) . show) "\n" x
                print_room xs

get_room :: Coords -> WorldMap -> Room
get_room pos@(x, y) world
    | is_inside pos map_size = world !! y !! x
    | otherwise              = error "Out of bounds"

is_inside :: Coords -> Coords -> Bool
is_inside (x, y) (a, b) = x < a && y < b && x >= 0 && y >=0

get_tile :: Coords -> Room -> Tile
get_tile pos@(x, y) room
    | is_inside pos room_size = room !! y !! x
    | otherwise               = error "Out of bounds!"
