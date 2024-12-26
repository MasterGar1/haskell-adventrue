{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Props where
import Objects

-- Map
get_seed_tile :: Coords -> Coords -> Int
get_seed_tile (l1, l2) (g1, g2) = (seed_global + seed_local) * (seed_global + seed_local + 1) `div` 2 + seed_local
    where
        seed_global = (g1 + g2) * (g1 + g2 + 1) `div` 2 + g2
        seed_local  = (l1 + l2) * (l1 + l2 + 1) `div` 2 + l1

get_absolute_coords :: Coords -> Coords -> Coords
get_absolute_coords (l1, l2) (g1, g2) = (g1 * fst room_size + l1, g2 * snd room_size + l2)

generate_entity :: Coords -> Coords -> Tile
generate_entity l@(l1, l2) g@(g1, g2)
    | randomness < 2  = O $ Chest $ select_item l g seed
    | randomness < 10 = O Wall
    | randomness < 15 = E $ select_enemy l g seed
    | otherwise       = O None
    where
        randomness = seed `mod` 100
        seed       = get_seed_tile l g

select_difficulty :: Coords -> Coords -> Int -> Double
select_difficulty l@(l1, l2) g@(g1, g2) seed = fromIntegral radius / fromIntegral max_rad
    where
        ms (p, q) = (p - 1, q - 1)
        (a, b)    = get_absolute_coords l g
        (x, y)    = get_absolute_coords (ms room_size) (ms map_size)
        radius    = a ^ 2 + b ^ 2
        max_rad   = x ^ 2 + y ^ 2

select_enemy :: Coords -> Coords -> Int -> Entity
select_enemy l@(l1, l2) g@(g1, g2) seed
    | coef < 0.3 = pick_random tier1_enemies seed
    | coef < 0.5 = pick_random tier2_enemies seed
    | coef < 0.7 = pick_random tier3_enemies seed
    | otherwise  = pick_random tier4_enemies seed
    where
        coef = select_difficulty l g seed

select_item :: Coords -> Coords -> Int -> Item
select_item l@(l1, l2) g@(g1, g2) seed
    | coef < 0.3 = pick_random tier1_items seed
    | coef < 0.5 = pick_random tier2_items seed
    | coef < 0.7 = pick_random tier3_items seed
    | otherwise  = pick_random tier4_items seed
    where
        coef = select_difficulty l g seed

generate_room :: Coords -> Room
generate_room global_coords =
    [ [ generate_entity global_coords (a, b) | b <- [0..snd room_size - 1] ]
                            | a <- [0..fst room_size - 1] ]

generate_map :: Coords -> WorldMap
generate_map (x, y) =
    [ [ generate_room (a, b) | b <- [0..x-1] ] | a <- [0..y-1] ]

-- Player
player :: Entity
player = Player 10 2 0 [basic_attack] [] ((0, 0), (2, 2))

tier1_enemies :: [Entity]
tier1_enemies = [imp, goblin, spirit]

tier2_enemies :: [Entity]
tier2_enemies = [orc, succubus, golem]

tier3_enemies :: [Entity]
tier3_enemies = [zephyr, vampire, ephmeral]

tier4_enemies :: [Entity]
tier4_enemies = [chimera, dragon, demon_lord]

-- Enemies
-- Tier 1
imp :: Entity
imp = Enemy 4 2 0 [claw_strike, lesser_recovery] "Imp"

goblin :: Entity
goblin = Enemy 6 1 1 [club_bash] "Goblin"

spirit :: Entity
spirit = Enemy 3 2 1 [possession] "Spirit"
--Tier 2
orc :: Entity
orc = Enemy 8 3 0 [club_bash, hardening] "Orc"

succubus :: Entity
succubus = Enemy 6 4 2 [claw_strike, recovery, charm] "Succubus"

golem :: Entity
golem = Enemy 12 1 4 [strike, smash] "Golem"
-- Tier 3
zephyr :: Entity
zephyr = Enemy 10 6 4 [wind_slash, howl] "Zephyr"

vampire :: Entity
vampire = Enemy 8 5 5 [bite, suck, blood_absorbtion] "Vampire"

ephmeral :: Entity
ephmeral = Enemy 6 5 6 [ghost_bullet, absorbtion] "Ephemeral"

-- Tier 4
chimera :: Entity
chimera = Enemy 15 8 6 [claw_strike, growth] "Chimera"

dragon :: Entity
dragon = Enemy 20 5 10 [fire_breath, bite, hardening] "Dragon"

demon_lord :: Entity
demon_lord = Enemy 25 10 5 [demon_claw, curse, greater_recovery] "Demon Lord"

-- Items
tier1_items :: [Item]
tier1_items = [skill_book_heal, health_potion, sword, tunic]

tier2_items :: [Item]
tier2_items = [skill_book_double_strike, skill_book_shred, 
                harming_potion, medium_health_potion, 
                chain_vest, long_sword]

tier3_items :: [Item]
tier3_items = [skill_book_medium_heal, skill_book_heavy_strike, skill_book_pierce, 
                withering_potion, defence_potion, weakness_potion,
                plate_armor, silver_sword, orc_heart]

tier4_items :: [Item]
tier4_items = [skill_book_high_heal, skill_book_true_slash, skill_book_thousand_cuts, 
                large_health_potion, iron_skin_potion, succubus_potion,
                dragon_slayer, mythril_armor, chimera_heart]

-- Passive items
sword :: Item
sword = Passive "Sword" (update (-1) 0 0 []) True

tunic :: Item
tunic = Passive "Tunic" (update 0 0 1 []) False

long_sword :: Item
long_sword = Passive "Long Sword" (update (-2) 0 0 []) True

chain_vest :: Item
chain_vest = Passive "Chain Vest" (update 0 0 2 []) False

silver_sword :: Item
silver_sword = Passive "Silver Sword" (update (-3) 0 0 []) True

orc_heart :: Item
orc_heart = Passive "Orc Heart" (update 2 0 0 []) False

plate_armor :: Item
plate_armor = Passive "Plate Armor" (update 0 0 3 []) False

dragon_slayer :: Item
dragon_slayer = Passive "Dragon Slayer" (update (-5) 0 0 []) True

mythril_armor :: Item
mythril_armor = Passive "Mythril Armor" (update 0 0 5 []) False

chimera_heart :: Item
chimera_heart = Passive "Chimera Heart" (update 5 0 0 []) False

-- Consumables
health_potion :: Item
health_potion = Consumeable "Small Health Potion" 1 (update 2 0 0 []) False

harming_potion :: Item
harming_potion = Consumeable "Harming Potion" 1 (update (-2) 0 0 []) True

weakness_potion :: Item
weakness_potion = Consumeable "Weakness Potion" 1 (update 0 (-1) 0 []) True

defence_potion :: Item
defence_potion = Consumeable "Defence Potion" 1 (update 0 0 1 []) False

medium_health_potion :: Item
medium_health_potion = Consumeable "Medium Health Potion" 1 (update 4 0 0 []) False

withering_potion :: Item
withering_potion = Consumeable "Withering Potion" 1 (update (-6) 0 0 []) True

succubus_potion :: Item
succubus_potion = Consumeable "Weakness Potion" 1 (update 0 (-2) 0 []) True

large_health_potion :: Item
large_health_potion = Consumeable "Large Health Potion" 1 (update 10 0 0 []) False

iron_skin_potion :: Item
iron_skin_potion = Consumeable "Iron Skin Potion" 1 (update 0 0 3 []) False

-- Skill books
skill_book_heal :: Item
skill_book_heal = Consumeable "Skill Book: Heal" 1 (update 0 0 0 [basic_heal]) False

skill_book_double_strike :: Item
skill_book_double_strike = Consumeable "Skill Book: Double Strike" 1 (update 0 0 0 [double_strike]) False

skill_book_medium_heal :: Item
skill_book_medium_heal = Consumeable "Skill Book: Medium Heal" 1 (update 0 0 0 [medium_heal]) False

skill_book_heavy_strike :: Item
skill_book_heavy_strike = Consumeable "Skill Book: Heavy Strike" 1 (update 0 0 0 [heavy_strike]) False

skill_book_high_heal :: Item
skill_book_high_heal = Consumeable "Skill Book: High Heal" 1 (update 0 0 0 [high_heal]) False

skill_book_true_slash :: Item
skill_book_true_slash = Consumeable "Skill Book: True Slash" 1 (update 0 0 0 [true_slash]) False

skill_book_thousand_cuts :: Item
skill_book_thousand_cuts = Consumeable "Skill Book: Thousand Cuts" 1 (update 0 0 0 [thousand_cuts]) False

skill_book_shred :: Item
skill_book_shred = Consumeable "Skill Book: Shred" 1 (update 0 0 0 [shred]) False

skill_book_pierce :: Item
skill_book_pierce = Consumeable "Skill Book: Pierce" 1 (update 0 0 0 [pierce]) False

-- Skills Player
basic_attack :: Skill
basic_attack = Offensive "Attack" (deal_damage 1)

basic_heal :: Skill
basic_heal = Defensive "Low Heal" (heal 0.7)

double_strike :: Skill
double_strike = Offensive "Double Strike" $ deal_damage 0.8 ... deal_damage 0.8

medium_heal :: Skill
medium_heal = Defensive "Medium Heal" (heal 1.2)

heavy_strike :: Skill
heavy_strike = Offensive "Heavy Strike" (deal_damage 1.5)

high_heal :: Skill
high_heal = Defensive "High Heal" (heal 2)

true_slash :: Skill
true_slash = Offensive "True Slash" (deal_damage 3)

thousand_cuts :: Skill
thousand_cuts = Offensive "Thousand Cuts"
            (deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9)

shred :: Skill
shred = Offensive "Shred" (\user -> update 0 0 1 [])

pierce :: Skill
pierce = Offensive "Shred" (\user -> update 0 0 3 [])

-- Skills Enemies
claw_strike :: Skill
claw_strike = Offensive "Claw Strike" (deal_damage 1)

lesser_recovery :: Skill
lesser_recovery = Defensive "Lesser Recovery" (heal 0.5)

club_bash :: Skill
club_bash = Offensive "Club Bash" (deal_damage 1)

possession :: Skill
possession = Offensive "Possession" (deal_damage 1.5)

hardening :: Skill
hardening = Defensive "Hardening" (\user -> update 0 0 1 [])

recovery :: Skill
recovery = Defensive "Recovery" (heal 0.7)

charm :: Skill
charm = Offensive "Charm" (\user -> update 0 1 0 [])


strike :: Skill
strike = Offensive "Strike" (deal_damage 1)

smash :: Skill
smash = Offensive "Smash" (deal_damage 2)

wind_slash :: Skill
wind_slash = Offensive "Wind Slash" (deal_damage 1)

howl :: Skill
howl = Defensive "Howl" (\user -> update 0 1 0 []) 

bite :: Skill
bite = Offensive "Bite" (deal_damage 1)

blood_absorbtion :: Skill
blood_absorbtion = Defensive "Blood Absorbtion" (heal 1.5)

suck :: Skill
suck = Offensive "Suck" (\user -> update 1 1 0 [])

ghost_bullet :: Skill
ghost_bullet = Offensive "Ghost Bullet" (\user -> update 1 0 1 [])

absorbtion :: Skill
absorbtion = Defensive "Absorbtion" (heal 1)

growth :: Skill
growth = Defensive "Growth" (\user -> update 1 1 1 [])

fire_breath :: Skill
fire_breath = Offensive "Fire Breath" (deal_damage 2)

demon_claw :: Skill
demon_claw = Offensive "Demon Claw" (deal_damage 1)

curse :: Skill
curse = Offensive "Curse" (\user -> update 0 1 1 [])

greater_recovery :: Skill
greater_recovery = Defensive "Greater Recovery" (heal 1)