{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Props where
import Objects

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
    | randomness < 2  = O $ Chest $ pick_random item_pool seed
    | randomness < 10 = O Wall
    | randomness < 15 = E $ pick_random enemy_pool seed
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

-- Entities
player :: Entity
player = Player 10 2 1 [basic_attack] [sword] ((0, 0), (2, 2))

enemy_pool :: [Entity]
enemy_pool = [imp]

imp :: Entity
imp = Enemy 4 2 0 [basic_attack] "Imp"

-- Skills
skill_pool :: [Skill]
skill_pool = [basic_attack, basic_heal]

basic_attack :: Skill
basic_attack = Offensive "Attack" (deal_damage 1)

basic_heal :: Skill
basic_heal = Defensive "Heal" (heal 0.7)

-- Items
item_pool :: [Item]
item_pool = [sword, health_potion]

sword :: Item
sword = Passive "Sword" $ update 1 0 0

health_potion :: Item
health_potion = Consumeable "Health Potion" 1 $ update 3 0 0
