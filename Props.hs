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
player = Player 10 3 0 [basic_attack] [] ((0, 0), (2, 2))

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
imp = Enemy 4 2 0 [claw_strike] "Imp"

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
tier1_items = [skill_book_heal, 
                health_potion, holy_water, 
                sword, tunic]

tier2_items :: [Item]
tier2_items = [skill_book_double_strike, skill_book_shred, 
                harming_potion, medium_health_potion, hard_flesh, 
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
sword = Passive "Sword" (update (-1) 0 0 []) True "Old rusty sword. Gives 1 bonus DMG"

tunic :: Item
tunic = Passive "Tunic" (update 0 0 1 []) False "Torn tunic. Gives 1 bonus DEF"

long_sword :: Item
long_sword = Passive "Long Sword" (update (-2) 0 0 []) True "Big sword. Gives 2 bonus DMG"

chain_vest :: Item
chain_vest = Passive "Chain Vest" (update 0 0 2 []) False "A knight's underarmor. Gives 2 bonus DEF."

silver_sword :: Item
silver_sword = Passive "Silver Sword" (update (-3) 0 0 []) True "A purified blade. Gives 3 bonus ATK."

orc_heart :: Item
orc_heart = Passive "Orc Heart" (update 2 0 0 []) False "It is still beating? Gives 2 bonus HP."

plate_armor :: Item
plate_armor = Passive "Plate Armor" (update 0 0 3 []) False "Simple armor. Gives 3 bonus DEF."

dragon_slayer :: Item
dragon_slayer = Passive "Dragon Slayer" (update (-5) 0 0 []) True "An almyghty sword. Gives 5 bonus ATK."

mythril_armor :: Item
mythril_armor = Passive "Mythril Armor" (update 0 0 5 []) False "Armor made of supreme materials. Gives 5 bonus DEF"

chimera_heart :: Item
chimera_heart = Passive "Chimera Heart" (update 5 0 0 []) False "This thing is grotesque... Gives 5 bonus HP."

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

hard_flesh :: Item
hard_flesh = Consumeable "Hard Flesh" 3 (update 0 1 0 []) False

holy_water :: Item
holy_water = Consumeable "Holy Water" 5 (update 1 0 0 []) False

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
basic_attack = Offensive "Attack" (deal_damage 1) "A basic attack of 100% ATK"

basic_heal :: Skill
basic_heal = Defensive "Low Heal" (heal 0.7) "A weak healing spell of 70% ATK"

double_strike :: Skill
double_strike = Offensive "Double Strike" (deal_damage 0.8 ... deal_damage 0.8) "Two hits of 80% ATK"

medium_heal :: Skill
medium_heal = Defensive "Medium Heal" (heal 1.2) "Healing spell of 120% ATK"

heavy_strike :: Skill
heavy_strike = Offensive "Heavy Strike" (deal_damage 1.5) "Large attack of 150% ATK"

high_heal :: Skill
high_heal = Defensive "High Heal" (heal 2) "Powerful heal of 200% ATK"

true_slash :: Skill
true_slash = Offensive "True Slash" (deal_damage 3) "An onmipotent strike of 300% ATK"

thousand_cuts :: Skill
thousand_cuts = Offensive "Thousand Cuts"
            (deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9
            ... deal_damage 0.9 ... deal_damage 0.9) "An endless barrage of 10 strikes of 90% ATK"

shred :: Skill
shred = Offensive "Shred" (\user -> update 0 0 1 []) "Break the enemy's DEF by 1"

pierce :: Skill
pierce = Offensive "Pierce" (\user -> update 0 0 3 []) "Crush the enemy's protection by 3 DEF"

-- Skills Enemies
claw_strike :: Skill
claw_strike = Offensive "Claw Strike" (deal_damage 1) "A claw attack of 100% ATK"

lesser_recovery :: Skill
lesser_recovery = Defensive "Lesser Recovery" (heal 0.5) "Tiny recovery spell of 50% ATK"

club_bash :: Skill
club_bash = Offensive "Club Bash" (deal_damage 1) "Smash with a club of 100% ATK"

possession :: Skill
possession = Offensive "Possession" (deal_damage 1.5) "Mental attack of 150% ATK"

hardening :: Skill
hardening = Defensive "Hardening" (\user -> update 0 0 1 []) "Protection spell + 1 DEF"

recovery :: Skill
recovery = Defensive "Recovery" (heal 0.7) "Small recovery of 70% ATK"

charm :: Skill
charm = Offensive "Charm" (\user -> update 0 1 0 []) "Reduction of 1 ATK"

strike :: Skill
strike = Offensive "Strike" (deal_damage 1) "Basic attack of 100% ATK"

smash :: Skill
smash = Offensive "Smash" (deal_damage 2) "Smash!!! of 200% ATK"

wind_slash :: Skill
wind_slash = Offensive "Wind Slash" (deal_damage 1) "Wind volley of 100% ATK"

howl :: Skill
howl = Defensive "Howl" (\user -> update 0 1 0 []) "Morale boost of 1 ATK"

bite :: Skill
bite = Offensive "Bite" (deal_damage 1) "Biting attack of 100% ATK"

blood_absorbtion :: Skill
blood_absorbtion = Defensive "Blood Absorbtion" (heal 1.5) "Absorb blood to heal by 150% of ATK"

suck :: Skill
suck = Offensive "Suck" (\user -> update 3 1 0 []) "Suck opponent's blood to take 3 HP and 1 ATK"

ghost_bullet :: Skill
ghost_bullet = Offensive "Ghost Bullet" (\user -> update 4 0 1 []) "An invisible attack reducing HP by 4 and DEF by 1"

absorbtion :: Skill
absorbtion = Defensive "Absorbtion" (heal 1) "Basic healing of 100% ATK"

growth :: Skill
growth = Defensive "Growth" (\user -> update 1 1 1 []) "Grow in all aspects!"

fire_breath :: Skill
fire_breath = Offensive "Fire Breath" (deal_damage 2) "Breathe fire to deal 200% of ATK"

demon_claw :: Skill
demon_claw = Offensive "Demon Claw" (deal_damage 1) "Tear all apart with 100% of ATK"

curse :: Skill
curse = Offensive "Curse" (\user -> update 0 1 1 []) "Weaken your opponent by 1 DEF and 1 ATK"

greater_recovery :: Skill
greater_recovery = Defensive "Greater Recovery" (heal 1) "Heal a lot by 100% of ATK"
