#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(purrr)

library(magick)

cards <- paste0(rep(c('B', 'R', 'S', 'E'), each = 9), rep(c(6:9, 'B', 'U', 'O', 'K', 'A'), 4))
d <- format(Sys.time(), '%Y-%m-%d')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$pic, {
        # clear file if it exists
        if(file.exists(input$file))
            file.remove(input$file)

        # take picture
        paste('ffmpeg -ss 0.5 -f avfoundation -framerate 30 -i "0" -t 1 -frames 1', input$file) %>%
            system()

        output$img <- renderPlot(
            {
                par(mar = rep(0, 4))

                if(file.exists(input$file))
                {
                    plot(image_read(input$file))
                }else{
                    plot(image_read('tmp.jpg'))
                }
            })
    })

    observeEvent(input$game, {
        # file name
        updateTextInput(session, 'file', value = paste0('../raw_hands/', d, '_',
                                                        input$game, '_', input$hand, '.jpg'))
    })

    observeEvent(input$hand, {
        # file name
        updateTextInput(session, 'file', value = paste0('../raw_hands/', d, '_',
                                                        input$game, '_', input$hand, '.jpg'))
    })

    observeEvent(input$sav, {
        cards_in_hand <- map_lgl(cards, ~ input[[.x]])

        if(sum(cards_in_hand) == 9)
        {
            cat(d, ',', input$game, ',', input$hand, ',',
                paste(as.integer(cards_in_hand), collapse = ','), '\n',
                file = '../raw_hands/hands.csv', sep = '', append = TRUE)
        }else{
            print(cards_in_hand)
            warning('expecting 9 cards')
        }
    })

    observeEvent(input$clear, {
        for(i in cards)
            updateCheckboxInput(session, i, value = FALSE)
    })

    # load and display picture
    output$img <- renderPlot(
    {
        par(mar = rep(0, 4))

        if(file.exists(input$file))
        {
            plot(image_read(input$file))
        }else{
            plot(image_read('tmp.jpg'))
        }
    })
})
