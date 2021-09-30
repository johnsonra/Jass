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

root <- system('git rev-parse --show-toplevel', intern = TRUE)
cards <- paste0(rep(c('B', 'R', 'S', 'E'), each = 9), rep(c(6:9, 'B', 'U', 'O', 'K', 'A'), 4))
d <- format(Sys.time(), '%Y-%m-%d')
images <- '/cards/'
placeholder_image <- '/shiny/tmp.jpg'
annotations <- 'cards.csv'

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observeEvent(input$pic, {
        # clear file if it exists
        if(file.exists(paste0(root, images, input$file)))
            file.remove(paste0(root, images, input$file))

        # take picture
        paste('ffmpeg -ss 0.5 -f avfoundation -framerate 30 -i "0" -t 1 -frames 1',
              paste0(root, images, input$file)) %>%
            system()

        output$img <- renderPlot(
            {
                par(mar = rep(0, 4))

                if(file.exists(paste0(root, images, input$file)))
                {
                    plot(image_read(paste0(root, images, input$file)))
                }else{
                    plot(image_read(paste0(root, placeholder_image)))
                }
            })
    })

    observeEvent(input$set, {
        # file name
        updateTextInput(session, 'file', value = paste0(d, '_', input$set, '_', input$card, '.jpg'))
    })

    observeEvent(input$card, {
        # file name
        updateTextInput(session, 'file', value = paste0(d, '_', input$set, '_', input$card, '.jpg'))
    })

    # observeEvent(input$sav, {
    #     #cards_in_hand <- map_lgl(cards, ~ input[[.x]])
    # 
    #     # if(sum(cards_in_hand) == 9)
    #     # {
    #         cat(d, ',', input$set, ',', input$card, '\n',
    #             file = paste0(root, images, annotations), sep = '', append = TRUE)
    #     # }else{
    #     #     print(cards_in_hand)
    #     #     warning('expecting 9 cards')
    #     # }
    # })

    observeEvent(input$clear, {
        for(i in cards)
            updateCheckboxInput(session, i, value = FALSE)
    })

    # load and display picture
    output$img <- renderPlot(
    {
        par(mar = rep(0, 4))

        if(file.exists(paste0(root, images, input$file)))
        {
            plot(image_read(paste0(root, images, input$file)))
        }else{
            plot(image_read(paste0(root, placeholder_image)))
        }
    })
})
