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

# Updates

## Architecture

### Data type module

This module defines basic data structures related to the game. 

**card** 

There are various attributes of a card. In this module, we first define data type describing a single card. Such data structure should include:

+ Value. The value of a card has a range from 11. For 'A', the value is stored as 11 but would be further checked to choose from 1 or 11 when actually making the calculation.
+ Character. The character on the card include 'A', 'J', 'Q', 'K' and '2' to '10'.
+ Suit. Four suits of a card also needs to be defined here.

**card pile**

Both the game player and the dealer have card piles, so we need to define a data structure of card piles as well. Notice that the status of card piles may not be the same. The dealer may have cards that are not visible to the player. Therefore, the card pile data structure shall have the following attributes:

+ Cards. A list of card that the pile contains.
+ Displaymode. Deciding whether only the character of the first card in the pile is visible to the player.

The UI module could use displaymode to render card piles. If the character of a card is not visible, the card will be drawn as a blank card. The Move module can fetch the data in cards to calculate value and change the current game status as well. 

### UI module

The UI interface is comprised of two card piles at the top and bottom respectively, representing the cards for both players, and two buttons at the right, one for 'add a card', the other for 'finish'. 

A card is consisted of the border, a number and the suit of it. To render it the techniques implemented in hw2(drawing pictures and document file trees) is used. Depending on the displaymode of a card, it might should either all the 3 elements or only the border of the card(signaling it's invisible to the player).

To render a card pile, the rightmost card is displayed completed, and other cards only displays their left-half borders, making it covered by other cards.

When the game ends, the card pile at the top side will show their values and a string in the middle of the board will show 'success' or 'fail', and a bottom below it signaling to continue a new game.

### Move module

* Click 'start game'

  two pairs of two cards will be moved from the card case to the front of the player and the dealer
  cards are faced down in card case, all of the cards in front of the player will face up while one card of the dealer is facing down and another one is facing up

* Click 'Hit'

  one card from the card case will be moved to the front of the player and place face up

  * when the sum of the cards is smaller than 21, the player could still press 'Hit' or 'Stand'
  * when the sum of the cards is equal to 21, turn to dealer round which is described in 4, and show the result according to the sum
  * when the sum of the cards is larger than 21, turns to dealer round which is described in 4 and shows 'player lose, dealer win'

* Click 'Stand'

  turns to dealer round which is described in 4, and show the result according to the sum

* Dealer's turn

  the facedown card of the dealer will turn up, and the dealer would ask for another facing-up card, after the dealer get the card, the result of the game would be shown

## Challenges

* We met a minor challenge yet it is still worthy of discussion. When designing how to draw a card, there is an unique card: card with value 10. It is troublesome because 10 has 2 characters, and all the designs must take this exception into account. To avoid unnecessary edge cases, we find that there is an unicode value that can draw '10' in a single character. in this way we reduced the complexity, along with designing the user interface in a more general approach.

* brick version, base version, and LTS compatibility

  when trying to use brick 0.26 on Ubuntu 18, the stack build showed that the base version is not compatible. So I tried to downgrade the ghc version, but encountered another problem: could not download 7.x.x ghc version to local. I changed the method by upgrading LTS version to a little higher version and also upgrading brick version a bit higher according to the version table. The building process then threw another error of lacking one lib file libtinfo.so related to vty. Finally, this problem was solved by this [post](https://github.com/commercialhaskell/stack/issues/1012).

## Goals expectation

With current progress, we think we can finish the original proposed goals on time. But the additional goals may not be implemented.
