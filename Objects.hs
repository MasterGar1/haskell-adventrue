{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Objects where

-- Some types
type Time = Integer
type Damage = Int
type Health = Damage
type Attack = Damage
type Defence = Damage
type Inventory = [Item]

type Effect = Entity -> Entity

-- Useful Data types
data Skill = Offensive { title :: String, func :: Effect } | Defensive { title :: String, func :: Effect }

data Entity = Enemy  { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], name :: String }
            | Player { health :: Health, attack :: Attack, defence :: Defence, skills :: [Skill], inventory :: Inventory }

data Item = Consumeable { label :: String, charges :: Int, effect :: Effect }
          | Passive     { label :: String, effect :: Effect }

data Interactable = None | Chest { loot :: Inventory }



-- Redefine classes
instance Show Skill where
    show (Offensive t _) = "Attack Skill: "  ++ t
    show (Defensive t _) = "Defence Skill: " ++ t

instance Show Item where
    show (Consumeable l c eff) = "Item: " ++ show l
                              ++ " | Charges: " ++ show c
    show (Passive l eff)       = "Item: " ++ show l

instance Eq Item where
    (Consumeable l1 _ _) == (Consumeable l2 _ _) = l1 == l2
    (Passive l1 _)       == (Passive l2 _)       = l1 == l2
    (Consumeable {})     == (Passive _ _)        = False

instance Show Entity where
    show (Enemy h a d s n)    = "Name: " ++ n ++ ['\n']
                             ++ "Health: " ++ show h
                             ++ " | Attack: " ++ show a
                             ++ " | Defence: " ++ show d ++ ['\n']
                             ++ "Skills: " ++ show (map title s)
    show (Player h a d s inv) = "Health: " ++ show h
                             ++ " | Attack: " ++ show a
                             ++ " | Defence: " ++ show d ++ ['\n']
                             ++ "Inventory: " ++ show inv ++ ['\n']
                             ++ "Skills: " ++ show (map title s)
                               

-- Entity updaters
update :: Entity -> Health -> Attack -> Defence -> Entity
update (Player _ _ _ sk inv) hp atk def = Player hp atk def sk inv
update (Enemy _ _ _ sk nm)   hp atk def = Enemy  hp atk def sk nm

-- Base Skills
heal :: Damage -> Effect
heal amount target
    | amount < 0 = error "Healing must be positive!"
    | otherwise  = update target final (attack target) (defence target)
    where
        final = amount + health target


deal_damage :: Damage -> Effect
deal_damage amount target
    | amount < 0 = error "Damage must be positive!"
    | otherwise  = update target final (attack target) (defence target)
    where
        final = max 0 (health target - max 0 (amount - defence target))

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
