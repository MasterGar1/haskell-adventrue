{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Props where
import Objects

-- Entities
player :: Entity
player = Player 10 2 1 [basic_attack] [sword] ((0, 0), (2, 2))

enemy_pool :: [Entity]
enemy_pool = [imp]

imp :: Entity
imp = Enemy 4 1 0 [basic_attack] "Imp"

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
health_potion = Consumeable "Health Potion" 1 $ update (-3) 0 0