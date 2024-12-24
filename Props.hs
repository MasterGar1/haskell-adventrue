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

-- >>> basic_attack player imp
-- Couldn't match expected type `Entity -> Entity -> t_a5nFz[sk:1]'
--             with actual type `Skill'
-- The function `basic_attack' is applied to two value arguments,
--   but its type `Skill' has none
-- In the expression: basic_attack player imp
-- In an equation for `it_a5nE0': it_a5nE0 = basic_attack player imp
-- Relevant bindings include
--   it_a5nE0 :: t_a5nFz[sk:1]
--     (bound at C:\Users\garig\Desktop\UNI\FP\haskell-adventrue\Props.hs:25:2)

-- Items
item_pool :: [Item]
item_pool = [sword, health_potion]

sword :: Item
sword = Passive "Sword" $ update 1 0 0

health_potion :: Item
health_potion = Consumeable "Health Potion" 1 $ update 3 0 0
