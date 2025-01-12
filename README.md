# Haskell Adventure

## Overview
Haskell Adventure is a text-based RPG game where you explore a world, encounter enemies, and collect items.

## Prerequisites
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) - Make sure you have GHC installed on your system.
- [Cabal](https://www.haskell.org/cabal/) - A system for building and packaging Haskell libraries and programs.

## Setup
1. **Clone the repository:**
    ```sh
    git clone https://github.com/MasterGar1/haskell-adventrue
    cd haskell-adventure
    ```

2. **Compile the project:**
    ```sh
    ghci Main.hs
    ```

3. **Run the game:**
    ```sh
    main
    ```

## How to Play
### Starting the Game
- Run the game using the command above.
- You will be greeted with a welcome message and prompted to type `start` to begin your journey or `help` to learn more about the gameplay.

### Game Commands
- **Movement:**
  - `left` or `l`: Move left
  - `right` or `r`: Move right
  - `up` or `u`: Move up
  - `down` or `d`: Move down

- **Information:**
  - `inventory` or `inv`: Check your inventory
  - `skills` or `sks`: Check your skills
  - `stats` or `sts`: Check your stats
  - `help` or `hp`: Display help menu

- **Combat:**
  - `attack` or `atk`: Choose a skill to attack
  - `item` or `itm`: Use a consumable item

- **Miscellaneous:**
  - `log`: Display the game log
  - `quit`: Exit the game

### Exploring the World
- Move around the map using the movement commands.
- Encounter enemies and chests as you explore.
- Enemies will trigger combat, while chests will give you items or skill books.

### Combat
- During combat, you can choose to attack using a skill or use an item.
- The combat screen will display your stats and the enemy's stats.
- The fight continues until one of the combatants is defeated.

### Inventory and Skills
- Check your inventory to see the items you have collected.
- Use consumable items to heal or buff your character.
- Passive items provide permanent stat increases.
- Check your skills to see the abilities you can use in combat.

### Help Menu
- Type `help` to access the help menu.
- The help menu provides information on combat, exploration, skills, and items.

## Game States
- **Start:** Initial state where you can start the game or access the help menu.
- **Explore:** Main game state where you move around and explore the world.
- **Fight:** Combat state where you engage in battles with enemies.
- **Help:** State where you can access various help topics.

## License
This project is licensed under the MIT License.
