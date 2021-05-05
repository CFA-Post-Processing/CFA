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
    inputId = "tableName", label = "Select data:",
    selected = NULL, multiple = FALSE,
    choices = c("Select Input", samples)
  ),
  )

# Server logic ----
server <- function(input, output) {
  
    dataOP <- reactive({
      inFile <- input$BAG1
      if(is.null(input$BAG1)){
        return(NULL)
      }
      #Identify_IP(read.table(inFile$datapath))
      list(tble = c(1:20), txt = c(1:10), plt = rnorm(100))
    })
    
    observeEvent(input$btn,{
      output$what <- renderTable({
        dataOP()$tble
      })
    })
    
    observeEvent(input$btn1,{
      output$pl <- renderPlot({
        plot(dataOP()$plt)
      })
    })              
  }


shinyApp(ui, server)
