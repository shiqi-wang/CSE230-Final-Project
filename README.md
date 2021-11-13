# CSE 230 - Final Project

## Blackjack

This project is aimed at implementing a Blackjack game based on Haskell and Brick library.

### Rules

The general object of the game is easy to understand: getting a total card score as close to 21 as possible, without going over 21. The score of each card is shown in this table:

|  card   |        score        |
| :-----: | :-----------------: |
|    A    |       1 or 11       |
|   2-9   | same as card number |
| J, Q, K |         10          |

The player and the virtual rival both get two cards at the beginning of the game, and the player can see one of the rival's cards. The player may choose not to ask for another card and "stand" on the two cards originally dealt to them, or may ask for additional cards ("hit"), one at a time, until deciding to "stand" on the total (if it is 21 or under), or goes "bust" (if it is over 21). In the latter case, the player loses. Otherwise, the rival follows the same procedure as the player. Finally, if the rival's score goes "bust" then the player wins, otherwise the one with a higher score is the winner.

### Goals

The general goal is to create a Blackjack game panel with command line user interface, implementing all the game rules described above. To be more specific, the features to implement include:

+ Basic game logic
+ User interface design and user input handling
+ Strategy of the virtual rival
+ (Additional) involving bets system in the game
+ (Additional) import networking modules to support online gaming

\* We will choose whether or not to include additional items based on actual development progress in the future.

### Members

* Yiming Mao(@[leafeonia](https://github.com/leafeonia))
* Shiqi Wang(@[shiqi-wang](https://github.com/shiqi-wang))
* Xiaoyang Wu(@[BryanWuxiaoyang](https://github.com/BryanWuxiaoyang))

### Links
Project requirements: https://ucsd-cse230.github.io/fa21/project.html<br>
Brick library: https://github.com/jtdaugherty/brick/