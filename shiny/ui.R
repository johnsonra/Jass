#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(purrr)

# Define UI for application that draws a histogram
dashboardPage(
    ################# Header #################
    dashboardHeader(title = "Jass", titleWidth = 400),

    ################# Sidebar ################
    dashboardSidebar(
        sidebarMenu(
            menuItem("Card Collection", tabName = 'annotate')
        )
    ),

    ################# Body ###################
    dashboardBody(tabItems(
        tabItem(tabName = 'annotate',

                # Image cards
                fluidRow(
                    column(3,
                           textInput('set', 'Set number:'),
                           textInput('card', 'Card:'),
                           textInput('file', '', value = 'tmp.jpg'),
                           actionButton('pic', 'Capture Hand')),
                    column(6,
                           plotOutput('img', height = '250px', width = '445px'))
                )
        )
    ))
)

