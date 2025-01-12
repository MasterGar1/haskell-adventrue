# Haskell Adventure

## Основна информация
Haskell Adventure (Не се сетих за по-добро име), е текстова приключенческа игра, спрямо условията, подадени [тук](https://docs.google.com/document/d/1fZO1vlMN5gNLE_C6MmWpbJ5HTWMxdkoyrYVvypgdJ2A/edit?tab=t.0).

Играта също е качена в [гитхъб репо](https://github.com/MasterGar1/haskell-adventrue), където може да се види кратка информация на ангкийски език и да се изтегли проекта при нужда.

## Информация за кода по модули
### Main.hs
В този модул се съдържа главния, игрален цикъл. Чрез него се обновяват всички главни обекти в играта.

### Objects.hs
Модул, съдържащ, всички използвани структури и типове в проекта, като играча, противниците, уменията и т.н.
Функцийте за работа с обектите, също се намират в този файл.

Интересен тип, са уменията (Skills). Те приемат източник (Entity) и приемник (Entity). Най-важната фукнция за тях е update.
Тя приема обект от тип Entity и прилага над него трансформация, по подадени модификатори.
Използването й в проекта се възползва от 'къринга' на Haskell, като приключва изпълнението и то фукнция то тип: Entity -> Entity
Това е нужно за да можем да преконфигурираме каквото искаме умение, без то да му подаваме използвател. По този начин,
постигаме умения, които могат да се използват от кое да е Entity. Също, функцията, (...) е оператор за композиране на умения, едно след друго, което прави системата доста лесна за разширяване и създаване на нови умения. (Много съм доволен, че се сетих да го направя така :)

От структурата Tile е съставена картата и той съдържа възможните за поставяне на поле елементи.
Предметите могат да са два типа - консумативи и пасивни. В зависимост от това имат различни предназначения:
- Пасивните се прилагат на играча на всеки ход, след което се премахват в края на хода
- Консумативите, могат да се използват на играча или на противник

В проекта съм използвал генератор на "произволни" числа (rand функцията). Тя изисква семе за да върне "произволно" число.
Защо 'произволно' е в кавички? Това е защото, може да се предскаже какво число ще се падне. Семето се генерира, като след всеки ход на играча се добавя 1 до досегашното семе. Поради това, може да има повторения на действията в две различни пускания на играта.
Формулата за псевдо-произволно число е взета от [тук](https://en.wikipedia.org/wiki/Random_number_generation), докато, чисените коефициенти са взаимствани от имплементацията на [srand](https://cplusplus.com/reference/cstdlib/srand/) в C++, която може да се види [тук](https://stackoverflow.com/questions/24005459/implementation-of-the-random-number-generator-in-c-c)

## Input.hs
В този модул се преработват входно-изходни данни, подадени от потребителя и се осъществява промяната в игралния свят, в зависимост от входа.

parse-input функцията е главната логика, която разпределя входовете към правилните места, след което връща резултата от тях върху света.

Тук съще се съдържат множество интерфейси и начини за извеждане на нужни на играча данни в даден момент. Може да забележете, че всички менюта в играта са 'окрасени', за да придават горе-долу приятен изглед в конзолата. Текста в тях е генериран с [този](https://patorjk.com/software/taag/#p=display&f=Cyberlarge&t=Haskell%20Adventure%0A) онлайн инструмент.

## Props.hs
Този модул съдържа всички константи елементи, които могат да се появят в картата. Също така, тук се намират и функцийте за генериране на света.

Интересната част в този модул, е създаването на картата. Това е реализирано, чрез използване на произволно семе, което се подава към rand от Objects модула. Семето се определя по следният начин:
Нека g(a, b) и l(x, y) са съответно координатите на стаята в картата и координатите на полето от стаята. 
Правим фукнция f(p, q) : N x N -> N, като:
    f(p, q) = (p + q) * (p + q + 1) / 2 + q
Тя лесно се доказва, че е биекция (примерно доказателство [тук](https://math.stackexchange.com/questions/3049970/proving-that-fn2-n-is-bijective)), от което знаем, че всеки образ на поле от картата ни дава различно естествеко число => различно семе. Подавайки absolute(a * room_size + x, b * room_size + y), които са абсолютните координати на поле от картата, като параметър на f, получаваме: seed = f(absolute)

След като знаем семето, генерираме вид обект в зависимост от дадани тежести, след което му избираме трудността, в зависимост от отдалечеността му от началото на картата (правим го с радиус вектор от началото на картата, който сравняваме с зададени радиуси на трудност).
Така получаваме произволна карта, която в зависимост от разстоянието което изминем от началото, слага все по-трудни противници и по-добри предмети.
ВАЖНО: Картата е еднаква при всяко започване на играта, поради това, че използва горепоказаният генератор на псевдо-произволни числа.

## Как се пуска проекта?
0. **(Опционално) Клониране на репото:**
    ```sh
    git clone https://github.com/MasterGar1/haskell-adventrue
    cd haskell-adventure
    ```

2. **Компилиране с ghcup:**
    ```sh
    ghci Main.hs
    ```

3. **Пускане на играта:**
    ```sh
    play
    ```

## Как да играем?
### Начало
При започване, получаваме малко информация за постановката на играта, след което можем да започнем играта с 'start', или да поискаме помощ с 'help'.
### Контроли
- **Движение:**
  - `left` или `l`: Ходене на ляво
  - `right` или `r`: Ходене на дясно
  - `up` или `u`: Ходене на горе
  - `down` или `d`: Ходене на долу

- **Информация:**
  - `inventory` или `inv`: Проверка на инвентара (Може да се показва информация, или да се използват предмети)
  - `skills` или `sks`: Проверка на уменията (Може да се вижда информация за уменията)
  - `stats` или `sts`: Проверка на статистиките
  - `help` или `hp`: Меню за помощ

- **Битки:**
  - `attack` или `atk`: Избор на умение
  - `item` или `itm`: Избор на използваем предмет

- **Други:**
  - `log`: Показване на историята на ходовете ни
  - `quit`: Изход от играта

### Ходене по картата
На картата можем да виждаме текущата стая и някои обекти, върху които ако стъпим ще се случи нещо:
- **Стена (#):** Блокира пътя на играча
- **Сандък (C):** Съдържа предмет
- **Противник (E):** Поле с противник
- **Играч (P):** Местоположението на играча
Ако излезем извън стая, отиваме в следващата.

### Битка
По време на битка може да атакуваме или да използваме предмет. На всеки ход, противника ни атакува независимо, от кое сме използвали.

## Информация за предмети и противници

### Предмети

#### Пасивни предмети
- **Sword**: Old rusty sword. Gives 1 bonus DMG.
- **Tunic**: Torn tunic. Gives 1 bonus DEF.
- **Long Sword**: Big sword. Gives 2 bonus DMG.
- **Chain Vest**: A knight's underarm. Gives 2 bonus DEF.
- **Silver Sword**: A purified blade. Gives 3 bonus ATK.
- **Orc Heart**: It is still beating? Gives 2 bonus HP.
- **Plate Armor**: Simple armor. Gives 3 bonus DEF.
- **Dragon Slayer**: An almighty sword. Gives 5 bonus ATK.
- **Mythril Armor**: Armor made of supreme materials. Gives 5 bonus DEF.
- **Chimera Heart**: This thing is grotesque... Gives 5 bonus HP.

#### Консумативи
- **Small Health Potion**: Heals 2 HP.
- **Harming Potion**: Deals 2 damage.
- **Weakness Potion**: Reduces 1 ATK.
- **Defense Potion**: Increases 1 DEF.
- **Medium Health Potion**: Heals 4 HP.
- **Withering Potion**: Deals 6 damage.
- **Succubus Potion**: Reduces 2 ATK.
- **Large Health Potion**: Heals 10 HP.
- **Iron Skin Potion**: Increases 3 DEF.
- **Hard Flesh**: Increases 1 ATK.
- **Holy Water**: Heals 1 HP.

#### Книги с умения
- **Skill Book: Heal**: Grants the skill `basic_heal`.
- **Skill Book: Double Strike**: Grants the skill `double_strike`.
- **Skill Book: Medium Heal**: Grants the skill `medium_heal`.
- **Skill Book: Heavy Strike**: Grants the skill `heavy_strike`.
- **Skill Book: High Heal**: Grants the skill `high_heal`.
- **Skill Book: True Slash**: Grants the skill `true_slash`.
- **Skill Book: Thousand Cuts**: Grants the skill `thousand_cuts`.
- **Skill Book: Shred**: Grants the skill `shred`.
- **Skill Book: Pierce**: Grants the skill `pierce`.

### Умения

#### Умения на играча
- **Basic Attack**: A basic attack of 100% ATK.
- **Low Heal**: A weak healing spell of 70% ATK.
- **Double Strike**: Two hits of 80% ATK.
- **Medium Heal**: Healing spell of 120% ATK.
- **Heavy Strike**: Large attack of 150% ATK.
- **High Heal**: Powerful heal of 200% ATK.
- **True Slash**: An omnipotent strike of 300% ATK.
- **Thousand Cuts**: An endless barrage of 10 strikes of 90% ATK.
- **Shred**: Break the enemy's DEF by 1.
- **Pierce**: Crush the enemy's protection by 3 DEF.

#### Умения на противниците
- **Claw Strike**: A claw attack of 100% ATK.
- **Lesser Recovery**: Tiny recovery spell of 50% ATK.
- **Club Bash**: Smash with a club of 100% ATK.
- **Possession**: Mental attack of 150% ATK.
- **Hardening**: Protection spell + 1 DEF.
- **Recovery**: Small recovery of 70% ATK.
- **Charm**: Reduction of 1 ATK.
- **Strike**: Basic attack of 100% ATK.
- **Smash**: Smash!!! of 200% ATK.
- **Wind Slash**: Wind volley of 100% ATK.
- **Howl**: Morale boost of 1 ATK.
- **Bite**: Biting attack of 100% ATK.
- **Blood Absorption**: Absorb blood to heal by 150% of ATK.
- **Suck**: Suck opponent's blood to take 3 HP and 1 ATK.
- **Ghost Bullet**: An invisible attack reducing HP by 4 and DEF by 1.
- **Absorption**: Basic healing of 100% ATK.
- **Growth**: Grow in all aspects!
- **Fire Breath**: Breathe fire to deal 200% of ATK.
- **Demon Claw**: Tear all apart with 100% of ATK.
- **Curse**: Weaken opponent by 1 DEF and 1 ATK.
- **Greater Recovery**: Heal a lot by 100% of ATK.

### Противници

#### Ниво 1
- **Imp**: 
  - **HP**: 4
  - **ATK**: 2
  - **DEF**: 0
  - **Skills**: `claw strike`
  - **Description**: Imp

- **Goblin**: 
  - **HP**: 6
  - **ATK**: 1
  - **DEF**: 1
  - **Skills**: `club bash`
  - **Description**: Goblin

- **Spirit**: 
  - **HP**: 3
  - **ATK**: 2
  - **DEF**: 1
  - **Skills**: `possession`
  - **Description**: Spirit

#### Ниво 2
- **Orc**: 
  - **HP**: 8
  - **ATK**: 3
  - **DEF**: 0
  - **Skills**: `clu _bash`, `hardening`
  - **Description**: Orc

- **Succubus**: 
  - **HP**: 6
  - **ATK**: 4
  - **DEF**: 2
  - **Skills**: `claw strike`, `recovery`, `charm`
  - **Description**: Succubus

- **Golem**: 
  - **HP**: 12
  - **ATK**: 1
  - **DEF**: 4
  - **Skills**: `strike`, `smash`
  - **Description**: Golem

#### Ниво 3
- **Zephyr**: 
  - **HP**: 10
  - **ATK**: 6
  - **DEF**: 4
  - **Skills**: `wind slash`, `howl`
  - **Description**: Zephyr

- **Vampire**: 
  - **HP**: 8
  - **ATK**: 5
  - **DEF**: 5
  - **Skills**: `bite`, `suck`, `blood absorption`
  - **Description**: Vampire

- **Ephemeral**: 
  - **HP**: 6
  - **ATK**: 5
  - **DEF**: 6
  - **Skills**: `ghost bullet`, `absorption`
  - **Description**: Ephemeral

#### Ниво 4
- **Chimera**: 
  - **HP**: 15
  - **ATK**: 8
  - **DEF**: 6
  - **Skills**: `claw strike`, `growth`
  - **Description**: Chimera

- **Dragon**: 
  - **HP**: 20
  - **ATK**: 5
  - **DEF**: 10
  - **Skills**: `fire breath`, `bite`, `hardening`
  - **Description**: Dragon

- **Demon Lord**: 
  - **HP**: 25
  - **ATK**: 10
  - **DEF**: 5
  - **Skills**: `demon claw`, `curse`, `greater recovery`
  - **Description**: Demon Lord
