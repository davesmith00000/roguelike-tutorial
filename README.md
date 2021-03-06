# Following the Roguelike Tutorial

This is an attempt to convert the python [Roguelike tutorial](http://rogueliketutorials.com/tutorials/tcod/) to Scala 3 using Indigo.

I'm following the spirit of the tutorials rather than the letter, since Indigo is an FP engine and the python version uses a lot of mutable constructs. Each part achieves the same goals, and where possible I've mimicked the algorithms and processes.

> The ["RoguelikeDev Does The Complete Roguelike Tutorial"](https://www.reddit.com/r/roguelikedev/comments/o5x585/roguelikedev_does_the_complete_roguelike_tutorial/) is now over for 2021! In celebration of making it to the finish line, [I've written up a brief post-mortem of the experience.](https://indigoengine.io/blog/2021/08/17/roguelike-post-mortem)

***UPDATE 27/11/2021 - A Fresh Perspective:*** For my own interest, I began revisiting the final code base that came out of my attempts to follow the tutorial. Couple of things I've noticed:

1. By the end, I really was just trying to finish the tutorials, so some of the code is not great.

2. One way in which I am following the tutorial fairly accurately (as I recall) is in the data modeling - maybe not identical by not thinking about it too hard because I was in a rush. The more I've tried to unpick the modeling to make it work with more functional code, the more I wish I'd thought about all this the first time around. It sort of looks ok, but it quickly unravels into a bit of a mess.

My advice would be to take the game model as a guideline only. These are the general things you'll need to think about... player, equipment, inventory, etc. But decide for yourself how they ought to relate to one another.

For example: In this code base the `Equipment` holds a weird link to the `Inventory` to know which inventory items are equipped. But from a data modeling perspective, if it's equipped... should it be in the inventory at all? If not, what happens to equipment management in terms of UI/UX?

## This is not a game

There are 13 parts to the tutorial (2020 version), all of which are presented below.

The final result of all this work is not a very good game. It isn't well balanced. It has no ending. The mechanics are limited. The code brilliantly well written - by the end I was just pleased to be finished!

All I've done it try and produce each section reasonably faithfully, but even there I've occasionally just made it up as I went along.

What this _might_ be, is the start of something. There are obvious improvements that could be made like giving the code a good refactoring, improving the rendering speed and reducing the system requirements. But even in it's current state.. it has promise, because... it's fun!

Maybe next year I'll start from where I left off. Or maybe you will? :-)

## Join in!

Everything is being built with [Indigo](https://indigoengine.io/) and I've made a [***roguelike starter kit***](https://github.com/PurpleKingdomGames/roguelike-starterkit) especially for use with Indigo, that you can use.

To follow along, you'll need to do a local publish of the starter kit, instructions on on the [repo's README.](https://github.com/PurpleKingdomGames/roguelike-starterkit)

## Completed Tutorial Parts

### Accumulated Controls

Parts|Controls
---|---
0/1 to 3|Move using Arrow keys, attack by bumping into baddies.
4 to 6|_As above, plus:_ Hit refresh in your browser to generate a new level.
7|_As above, plus:_ Hit the 'v' key to show message history and use up and down arrows to scroll.
8|_As above, plus:_ Hit the 'i' key to show inventory and 'd' to show the "drop" menu. Use up and down arrows to scroll in both, the a-z keys to choose an item to consume/drop, and shift|ctrl|alt|esc to close any windows.
9|_As above, plus:_ Hit the "/" key to enter "look around" mode and "/" or "Esc" to leave. When using a confusion or fireball scroll, you will enter look around mode to find a target, press "Enter" to select it.
10|_As above, plus:_ Hit "n" to start a new game or "c" to continue a saved game on the main menu. In game, hit "q" to bring up the quit menu, choose / press "1", "2", or "3" to save your game, save and quit, or quit to main menu respectively. When you die, you can still hit "q" to bring up the menu and return to the main menu.
11 to 13|_As above, plus:_ Hit "c" to show/hide the character stats window, when standing on stairs, press "." to go down a level. In the level up menu, choose the stat to increase with 1, 2, or 3 keys.

### Table of Contents

Part|Title|Screenshot(s)|Play!
---|---|---|---
0/1|Setting Up / Drawing the ???@??? symbol and moving it around|![Part 1](part1/roguelike_part1.gif "Part 1")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part1/)
2|The generic Entity, the render functions, and the map|![Part 2](part2/roguelike-part2.gif "Part 2")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part2/)
3|Generating a dungeon|![Part 3](part3/roguelike-part3_2.gif "Part 3")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part3/)
4|Field of view|![Part 4](part4/roguelike-part4_2.gif "Part 4")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part4/)
5|Placing enemies and kicking them (harmlessly)|![Part 5](part5/roguelike_part5.gif "Part 5")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part5/)
6|Doing (and taking) some damage|![Part 6](part6/roguelike_part6.gif "Part 6")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part6/)
7|Creating the Interface|![Part 7](part7/roguelike_part7_2.gif "Part 7")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part7/)
8|Items and Inventory|![Part 8](part8/roguelike_part8.gif "Part 8")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part8/)
9|Ranged Scrolls and Targeting|![Part 9](part9/roguelike_part9.gif "Part 9")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part9/)
10|Saving and loading|![Part 10](part10/roguelike_part10.gif "Part 10")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part10/)
11|Delving into the Dungeon|![Part 11](part11/roguelike_part11.gif "Part 11")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part11/)
12|Increasing Difficulty|![Part 12](part12/roguelike_part12.gif "Part 12")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part12/)
13|Gearing up|![Part 13](part13/roguelike_part13.gif "Part 13")|[Click to play!](https://davesmith00000.github.io/roguelike-tutorial/part13/)
