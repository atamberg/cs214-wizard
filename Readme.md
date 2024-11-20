# Team 64

## Team members
```
 - Bernardi Leoluca        - 374107, leoluca.bernardi@epfl.ch
 - Gorgani Ali             - 371956, ali.gorgani@epfl.ch
 - Metzler Mattia Gaël     - 372025, mattia.metzler@epfl.ch
 - Tamberg Anthony Andreas - 357610, anthony.tamberg@epfl.ch
```

## Proposal
We are recreating the card game ['Wizard'](https://en.wikipedia.org/wiki/Wizard_(card_game)), described in the following.

### User stories
- As a player during the bidding phase, I want to be able to announce how many tricks I think I am going to make during the round.
- During the round, as a player, I want to play a legal card when it's my turn.
- At any time, I want to see my score, the current trump card and my current hand. I also want to see the other players, to know whose turn it is and to see how close they are to their goal of bids.
- As a player, at the end of a round, I want to see who won the trick and what the resulting points are. At the end of the game, I want to see who won with how many points.
- As a player, I don't want other players to see my cards.

### Requirements
- Entering a bid should save and display it next to the user's name for that round.
- Currently playable cards should be highlighted clearly and non-playable ones should not be clickable.
- Clicking a playable card should play the card by putting it in the middle with the other cards.
- The number of cards in the first round should be one.
- Once all cards have been played, the round should stop. The cards will be reshuffled and the number of cards for the next round should increment by one.
- The amount of cards in the last round is 60 divided by the number of players. After this round, the game should end and the scores should be displayed.
- Throughout the game, a counter should keep track of the scores.

### Mock-up
Bidding phase:
![bidding](./images/betting.png)

Turn of the player seeing this view:
![your turn](./images/your_turn.png)

Turn of some other player:
![other turn](./images/other_turn.png)

End of the game:
![end of game](./images/end_of_game.png)

### Roles
```
Ali:     Intergration & Testing
Anthony: Backend
Leoluca: Backend / Integration & Testing
Mattia:  Frontend
```
