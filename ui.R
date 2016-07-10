library(shiny)
library(ggplot2)
is.clicked = FALSE

# Define UI for application 
shinyUI(

  navbarPage(
    theme = 'bootstrap.css',
    userText = "", 
    fluid = TRUE, 
    title = "Tenacious Terrier Text Prediction", 
    tabPanel("Word Predictor"
    , sidebarLayout(
      sidebarPanel(
        imageOutput("logo", inline = TRUE)
        , br()
        , br()
        , textInput("userText", label = "Input Your Text Below: ", value = "Let me predict words for you...", width = '100%') 
        , br()
        , br()
        , strong("Cloud of Next Word Predictions:")
        , plotOutput("cloud", width = '100%')
        )
        
      , mainPanel(
        plotOutput("topWords", width = '100%') 
        , em("Top 5 most likely next words based on model predictions.")
        , br()
        , br()
        , plotOutput("accuracy", width = '100%')
        , em("Cumulative percentage of words in text occurring in top 3 most likely next predicted words.")
      )
    )
    )
  , a(href="https://s3.amazonaws.com/coursera-uploads/peer-review/2y_2_3REEeWKsgrp3VnvAw/8f20c1a67eff4c34598ac1d10a06ea5d/TenaciousTerrierApp-rpubs.html#/", "About the Predictor")
  , br()
  , br()
  )
)  
