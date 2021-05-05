#----------------------------------------------------------------------------------------
# Program name: app.R
# Authors: Azzurra Spagnesi
# Date: May 2021
# Objective: Import data for abakus, conductivity, draw wire and flowmeter calling
#            CFA_data_GC.R script. Manage statistics and plot the results within the app
#----------------------------------------------------------------------------------------
#clean the virtual work folder
rm(list = ls()) 
#install.packages("shiny")
library(shiny)
setwd("C:/Users/azzur/Dropbox/Il mio PC (LAPTOP-AUDRMQN9)/Desktop/CFA_data")
source('CFA_data_GC.R') #source must be outside the ui and server function

# User interface ----
ui<- fluidPage(
  # Application title
  titlePanel("Continuous Flow Analysis"),
  # Listing DFs
  selectInput(
    inputId = "tableName", label = "Sample:",
    selected = NULL, multiple = FALSE,
    choices = c("Select sample", SAMPLE)
  ),
  uiOutput("secondSelection")
)

# Server logic ----
server <- function(input, output){
  output$secondSelection <- renderUI({
    selectInput(
      inputId = "tableName", label = "Data:",
      selected = NULL, multiple = FALSE,
      choices = c("choose", SAMPLE$samples)
    )})
}


shinyApp(ui, server)
