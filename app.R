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

runApp(list(
  ui = fluidPage(
    titlePanel("CFA"),
    sidebarLayout(
      sidebarPanel(
        selectInput("sample",label = "sample",choices = c("BAG1", "BAG2")),
        selectInput("data",label = "data",choices = "")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  
  , server = function(input,output,session) {
    observe({
      if (input$sample == "BAG1") {
        choices <- c("BAG1_abakus","BAG1_cond","BAG1_flow","BAG1_IceHeight","BAG1_MeltSpeed")
      }
      else {
        choices <- c("BAG2_abakus","BAG2_cond","BAG2_flow","BAG2_IceHeight","BAG2_MeltSpeed",
                     "Cond_NormSmooth_peakArea","Cond_NormSmooth_peaks","DELAYS")
      }
      updateSelectInput(session,"data",choices=choices,selected=choices[1])
    })
    
    output$plot <- renderPlot({
      if (input$data != "") {
        plot(runif(as.numeric(input$data)),col=isolate(input$sample))
      }
    })
  })
)
