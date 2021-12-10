Jass
====

The Jass package contains the tools to play a game of Jass with one or
more AI players.

The `training` branch contains a deep learning model to recognize cards,
allowing the user to deal a physical hand of cards to a mix of real and
AI players. It will also contain deep learning models for AI players.

The `annotation` branch contains a shiny app for input and annotation of
image data needed for training of the card recognition model. Source
images will be linked here at some point.

Not all of the names and terminology I use is standard, but I’ve tried
to stick to the official rules.

Installation
------------

    library(devtools)

    install_github('johnsonra/Jass')

A sample game
-------------

For now, the AI players only know how to play a valid card. It is little
better than playing with a ghost hand, but the parts are in place to
start working on smarter AI models.

Let’s start by initializing a game of Cross Jass (this is what I always
heard it called, although it is also called Schieber Jass).

    # load packages
    library(Jass)
    library(magrittr)

    # set the random seed so we get the same cards each time
    set.seed(2389756)

    g <- new('Game') %>%
      deal()

Now, lets see what cards we have in our hand.

    cards(g)

    ##      suit face trump inhand
    ## 1   Bells    A FALSE   TRUE
    ## 2   Bells    B FALSE   TRUE
    ## 3   Bells    9 FALSE   TRUE
    ## 4   Bells    7 FALSE   TRUE
    ## 5 Flowers    8 FALSE   TRUE
    ## 6 Shields    B FALSE   TRUE
    ## 7 Shields    6 FALSE   TRUE
    ## 8  Acorns    B FALSE   TRUE
    ## 9  Acorns    7 FALSE   TRUE

Let’s pick Bells for trump (i.e. Schalen - my kids call them Bells).

    trump(g) <- 'Bells'
