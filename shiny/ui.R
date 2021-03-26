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
            menuItem("Data Collection", tabName = 'annotate')
        )
    ),

    ################# Body ###################
    dashboardBody(tabItems(
        tabItem(tabName = 'annotate',

                # Image hand
                fluidRow(
                    column(3,
                           textInput('game', 'Game number:'),
                           textInput('hand', 'Hand number:'),
                           textInput('file', '', value = 'tmp.jpg'),
                           actionButton('clear', 'Clear Hand'),
                           actionButton('pic', 'Capture Hand'),
                           actionButton('sav', 'Save Hand')),
                    column(6,
                           plotOutput('img', height = '250px', width = '445px'))
                ),

                # Cards
                fluidRow(h2("Cards in this hand")),
                fluidRow(
                    column(1),
                    column(2,
                           h3("Schellen"),
                           checkboxInput('B6', '6'),
                           checkboxInput('B7', '7'),
                           checkboxInput('B8', '8'),
                           checkboxInput('B9', '9'),
                           checkboxInput('BB', 'B'),
                           checkboxInput('BU', 'U'),
                           checkboxInput('BO', 'O'),
                           checkboxInput('BK', 'K'),
                           checkboxInput('BA', 'A')),
                    column(2,
                           h3("Rosen"),
                           checkboxInput('R6', '6'),
                           checkboxInput('R7', '7'),
                           checkboxInput('R8', '8'),
                           checkboxInput('R9', '9'),
                           checkboxInput('RB', 'B'),
                           checkboxInput('RU', 'U'),
                           checkboxInput('RO', 'O'),
                           checkboxInput('RK', 'K'),
                           checkboxInput('RA', 'A')),
                    column(2,
                           h3("Schilten"),
                           checkboxInput('S6', '6'),
                           checkboxInput('S7', '7'),
                           checkboxInput('S8', '8'),
                           checkboxInput('S9', '9'),
                           checkboxInput('SB', 'B'),
                           checkboxInput('SU', 'U'),
                           checkboxInput('SO', 'O'),
                           checkboxInput('SK', 'K'),
                           checkboxInput('SA', 'A')),
                    column(2,
                           h3("Eicheln"),
                           checkboxInput('E6', '6'),
                           checkboxInput('E7', '7'),
                           checkboxInput('E8', '8'),
                           checkboxInput('E9', '9'),
                           checkboxInput('EB', 'B'),
                           checkboxInput('EU', 'U'),
                           checkboxInput('EO', 'O'),
                           checkboxInput('EK', 'K'),
                           checkboxInput('EA', 'A'))
                )
        )
    ))
)

