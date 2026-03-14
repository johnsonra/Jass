

# Jass

The Jass package contains the tools to play a game of Jass with one or
more AI players.

The `training` branch contains a deep learning model to recognize cards,
allowing the user to deal a physical hand of cards to a mix of real and
AI players. It will also contain deep learning models for AI players.

The `annotation` branch contains a shiny app for input and annotation of
image data needed for training of the card recognition model. Source
images will be linked here at some point.

Not all of the names and terminology I use is standard, but I’ve tried
to stick to the official rules ([see this
site](https://www.swisslos.ch/en/jass/informations/jass-rules/schieber-jass.html)).

## Installation

``` r
devtools::install_github('johnsonra/Jass')
```

## A sample game

For now, the AI players only know how to play a valid card. It is little
better than playing with a ghost hand, but the parts are in place to
start working on smarter AI models.

Let’s start by initializing a game of Cross Jass (this is what I always
heard it called, although it is also called Schieber Jass).

``` r
# load package
library(Jass)
```

``` r
# set the random seed so we get the same cards each time
set.seed(2389756)

g <- new('Game') |>
  set_human(1) |>    # we'll play for player #1 all other players are AI by default
  deal()
```

Now, lets see what cards we have in our hand. By default the first
player’s cards are shown.

``` r
cards(g)
```

         suit face trump lead_suit
    1  Acorns    B FALSE        NA
    2  Acorns    7 FALSE        NA
    3   Bells    A FALSE        NA
    4   Bells    B FALSE        NA
    5   Bells    9 FALSE        NA
    6   Bells    7 FALSE        NA
    7 Flowers    8 FALSE        NA
    8 Shields    B FALSE        NA
    9 Shields    6 FALSE        NA

Looks like Bells is a decent choice for trump (i.e. Schellen).

``` r
trump(g) <- 'Bells'

# cards are reordered according to trump precedence
cards(g)
```

         suit face trump lead_suit
    1   Bells    9  TRUE        NA
    2   Bells    A  TRUE        NA
    3   Bells    B  TRUE        NA
    4   Bells    7  TRUE        NA
    5  Acorns    B FALSE        NA
    6  Acorns    7 FALSE        NA
    7 Flowers    8 FALSE        NA
    8 Shields    B FALSE        NA
    9 Shields    6 FALSE        NA

Before the first card is played, we can check for melds (Stock and Wys).
Each player declares any sequences of three or more cards in the same
suit, four-of-a-kind combinations, or a marriage (King + Ober in the
trump suit). Only the team with the highest individual meld scores all
of their combined meld points.

``` r
meld_result <- meld(g)

# See what each player can declare
lapply(meld_result$player_melds, function(m) {
  if(length(m) == 0) return("No melds")
  sapply(m, function(x) paste(x$type, x$points, 'pts'))
})
```

    [[1]]
    [1] "No melds"

    [[2]]
    [1] "No melds"

    [[3]]
    [1] "No melds"

    [[4]]
    [1] "marriage 20 pts"

``` r
# Which team scores, and how much?
meld_result$team_scores
```

    Team 1 Team 2 
         0     20 

We’ll jump right in with the first trick.

``` r
# play Ace of Bells
g <- play(g, 'BA')
```

Now it is on to the AI players.

``` r
# player 2 (AI) (we'll set auto to FALSE to only play one card here)
g <- play(g, auto = FALSE)

# player 3 (AI)
g <- play(g, auto = FALSE)

# player 4 (AI)
g <- play(g, auto = FALSE)

# end of trick
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1   Bells    U  TRUE      TRUE      3
    2   Bells    A  TRUE      TRUE      1
    3   Bells    O  TRUE      TRUE      4
    4 Shields    K FALSE     FALSE      2

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
         0      0 

    Current game score:
    Team 1 Team 2 
         0      0 

``` r
g <- next_trick(g)
status(g)
```

    Cards on the table:
    [1] suit      face      trump     lead_suit player   
    <0 rows> (or 0-length row.names)

    Next player: 3

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        38      0 

    Current game score:
    Team 1 Team 2 
        38      0 

Now that we see the general flow, lets auto-play the first half of this
trick.

``` r
# play for players 3 and 4
g <- play(g, auto = TRUE)

# check status and play
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Shields    A FALSE      TRUE      3
    2 Shields    U FALSE      TRUE      4

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        38      0 

    Current game score:
    Team 1 Team 2 
        38      0 

``` r
cards(g)
```

         suit face trump lead_suit
    1   Bells    9  TRUE     FALSE
    2   Bells    B  TRUE     FALSE
    3   Bells    7  TRUE     FALSE
    4 Shields    B FALSE      TRUE
    5 Shields    6 FALSE      TRUE
    6  Acorns    B FALSE     FALSE
    7  Acorns    7 FALSE     FALSE
    8 Flowers    8 FALSE     FALSE

``` r
g <- play(g, 'SB')

# continue play
g <- play(g, auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Shields    A FALSE      TRUE      3
    2 Shields    U FALSE      TRUE      4
    3 Shields    B FALSE      TRUE      1
    4 Shields    9 FALSE      TRUE      2

    Next player: 3

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        38      0 

    Current game score:
    Team 1 Team 2 
        38      0 

``` r
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1  Acorns    U FALSE      TRUE      3
    2 Shields    O FALSE     FALSE      4

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        61      0 

    Current game score:
    Team 1 Team 2 
        61      0 

This has taken us through the end of the second trick. Player 3 has won
the second trick again and started the third trick. We’ll just zoom
ahead now.

``` r
cards(g)
```

         suit face trump lead_suit
    1   Bells    9  TRUE     FALSE
    2   Bells    B  TRUE     FALSE
    3   Bells    7  TRUE     FALSE
    4  Acorns    B FALSE      TRUE
    5  Acorns    7 FALSE      TRUE
    6 Flowers    8 FALSE     FALSE
    7 Shields    6 FALSE     FALSE

``` r
g <- play(g, 'A7', auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1  Acorns    O FALSE      TRUE      2
    2  Acorns    U FALSE      TRUE      3
    3  Acorns    7 FALSE      TRUE      1
    4 Shields    O FALSE     FALSE      4

    Next player: 3

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        61      0 

    Current game score:
    Team 1 Team 2 
        61      0 

``` r
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1  Acorns    A FALSE      TRUE      2
    2  Acorns    6 FALSE      TRUE      3
    3 Flowers    A FALSE     FALSE      4

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        61      8 

    Current game score:
    Team 1 Team 2 
        61      8 

``` r
cards(g)
```

         suit face trump lead_suit
    1   Bells    9  TRUE     FALSE
    2   Bells    B  TRUE     FALSE
    3   Bells    7  TRUE     FALSE
    4  Acorns    B FALSE      TRUE
    5 Flowers    8 FALSE     FALSE
    6 Shields    6 FALSE     FALSE

``` r
g <- play(g, 'B7')

g <- next_trick(g)
g <- play(g, 'F8', auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1   Bells    8  TRUE     FALSE      3
    2 Flowers    B FALSE      TRUE      4
    3 Flowers    9 FALSE      TRUE      2
    4 Flowers    8 FALSE      TRUE      1

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        83      8 

    Current game score:
    Team 1 Team 2 
        83      8 

``` r
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Shields    8 FALSE      TRUE      4
    2 Shields    7 FALSE      TRUE      3

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        93      8 

    Current game score:
    Team 1 Team 2 
        93      8 

``` r
cards(g)
```

         suit face trump lead_suit
    1   Bells    9  TRUE     FALSE
    2   Bells    B  TRUE     FALSE
    3 Shields    6 FALSE      TRUE
    4  Acorns    B FALSE     FALSE

``` r
g <- play(g, 'S6', auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Shields    8 FALSE      TRUE      4
    2 Shields    7 FALSE      TRUE      3
    3 Shields    6 FALSE      TRUE      1
    4 Flowers    U FALSE     FALSE      2

    Next player: 3

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        93      8 

    Current game score:
    Team 1 Team 2 
        93      8 

``` r
status(g)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    7 FALSE      TRUE      4

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        93     10 

    Current game score:
    Team 1 Team 2 
        93     10 

``` r
cards(g)
```

        suit face trump lead_suit
    1  Bells    9  TRUE     FALSE
    2  Bells    B  TRUE     FALSE
    3 Acorns    B FALSE     FALSE

``` r
g <- play(g, 'BB', auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1   Bells    B  TRUE     FALSE      1
    2 Flowers    O FALSE      TRUE      3
    3 Flowers    7 FALSE      TRUE      4
    4 Flowers    6 FALSE      TRUE      2

    Next player: 4

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
        93     10 

    Current game score:
    Team 1 Team 2 
        93     10 

``` r
status(g)
```

    Cards on the table:
    [1] suit      face      trump     lead_suit player   
    <0 rows> (or 0-length row.names)

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
       106     10 

    Current game score:
    Team 1 Team 2 
       106     10 

``` r
cards(g)
```

        suit face trump lead_suit
    1  Bells    9  TRUE        NA
    2 Acorns    B FALSE        NA

``` r
g <- play(g, 'AB', auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1  Acorns    K FALSE      TRUE      3
    2  Acorns    B FALSE      TRUE      1
    3  Acorns    9 FALSE      TRUE      2
    4 Flowers    K FALSE     FALSE      4

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
       106     10 

    Current game score:
    Team 1 Team 2 
       106     10 

``` r
status(g)
```

    Cards on the table:
       suit face trump lead_suit player
    1 Bells    K  TRUE      TRUE      4
    2 Bells    6  TRUE      TRUE      3

    Next player: 1

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
       124     10 

    Current game score:
    Team 1 Team 2 
       124     10 

``` r
cards(g)
```

       suit face trump lead_suit
    1 Bells    9  TRUE      TRUE

``` r
g <- play(g, 'B9', auto = TRUE)
```

    Cards on the table:
        suit face trump lead_suit player
    1  Bells    9  TRUE      TRUE      1
    2  Bells    K  TRUE      TRUE      4
    3  Bells    6  TRUE      TRUE      3
    4 Acorns    8 FALSE     FALSE      2

    Next player: 3

    Trump suit: Bells

    Current score for the round:
    Team 1 Team 2 
       124     10 

    Current game score:
    Team 1 Team 2 
       124     10 

``` r
status(g)
```

    Cards on the table:
    [1] suit      face      trump     lead_suit player   
    <0 rows> (or 0-length row.names)

    Next player: 2

    Trump suit: 

    Current score for the round:
    Team 1 Team 2 
         0      0 

    Current game score:
    Team 1 Team 2 
       147     10 

Next, lets try a fully automated round.

``` r
set.seed(923847)
g2 <- new('Game') |>
  deal()

# pick trump
trump(g2) <- pick_random_trump(g2, 2)

g2 <- play(g2, auto = TRUE)
```

    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    9  TRUE     FALSE      2
    2 Shields    A FALSE      TRUE      1
    3 Shields    O FALSE      TRUE      3
    4 Shields    7 FALSE      TRUE      4

    Next player: 1

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
         0      0 

    Current game score:
    Team 1 Team 2 
         0      0 
    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    U  TRUE      TRUE      2
    2 Flowers    K  TRUE      TRUE      1
    3 Flowers    8  TRUE      TRUE      4
    4 Flowers    7  TRUE      TRUE      3

    Next player: 2

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
         0     28 

    Current game score:
    Team 1 Team 2 
         0     28 
    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    6  TRUE     FALSE      1
    2 Shields    K FALSE      TRUE      4
    3 Shields    B FALSE      TRUE      2
    4  Acorns    9 FALSE     FALSE      3

    Next player: 2

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
         0     52 

    Current game score:
    Team 1 Team 2 
         0     52 
    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    O  TRUE      TRUE      3
    2 Flowers    B  TRUE      TRUE      1
    3  Acorns    K FALSE     FALSE      4
    4  Acorns    O FALSE     FALSE      2

    Next player: 1

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        14     52 

    Current game score:
    Team 1 Team 2 
        14     52 
    Cards on the table:
         suit face trump lead_suit player
    1 Flowers    A  TRUE     FALSE      1
    2  Acorns    U FALSE      TRUE      2
    3  Acorns    8 FALSE      TRUE      3
    4  Acorns    7 FALSE      TRUE      4

    Next player: 3

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        34     52 

    Current game score:
    Team 1 Team 2 
        34     52 
    Cards on the table:
         suit face trump lead_suit player
    1  Acorns    A FALSE      TRUE      1
    2  Acorns    6 FALSE      TRUE      3
    3   Bells    O FALSE     FALSE      4
    4 Shields    8 FALSE     FALSE      2

    Next player: 1

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        47     52 

    Current game score:
    Team 1 Team 2 
        47     52 
    Cards on the table:
         suit face trump lead_suit player
    1 Shields    9 FALSE      TRUE      2
    2 Shields    6 FALSE      TRUE      1
    3   Bells    U FALSE     FALSE      4
    4   Bells    7 FALSE     FALSE      3

    Next player: 1

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        61     52 

    Current game score:
    Team 1 Team 2 
        61     52 
    Cards on the table:
       suit face trump lead_suit player
    1 Bells    A FALSE      TRUE      1
    2 Bells    K FALSE      TRUE      2
    3 Bells    B FALSE      TRUE      3
    4 Bells    9 FALSE      TRUE      4

    Next player: 2

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        61     54 

    Current game score:
    Team 1 Team 2 
        61     54 
    Cards on the table:
         suit face trump lead_suit player
    1 Shields    U FALSE      TRUE      1
    2  Acorns    B FALSE     FALSE      3
    3   Bells    8 FALSE     FALSE      4
    4   Bells    6 FALSE     FALSE      2

    Next player: 1

    Trump suit: Flowers

    Current score for the round:
    Team 1 Team 2 
        86     54 

    Current game score:
    Team 1 Team 2 
        86     54 

``` r
status(g2)
```

    Cards on the table:
    [1] suit      face      trump     lead_suit player   
    <0 rows> (or 0-length row.names)

    Next player: 2

    Trump suit: 

    Current score for the round:
    Team 1 Team 2 
         0      0 

    Current game score:
    Team 1 Team 2 
       103     54 
