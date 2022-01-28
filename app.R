#' ---
#' title: 'Task 06: Data Product'
#' author: "Jeffrey Strickland"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
 
#' ## Shiny App 
#' This script creates a Shiny App that takes a word or phrase input in a text box
#' and outputs the a predicted next word. 

library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(rintrojs)
})

#' Source ngram matching function
source("ngram.R")

#' Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction Model"),
  p("This app that takes an input phrase (multiple words) in a text box and outputs a prediction of the next word."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2("Instructions:"), 
      h5("1. Enter a word or words in the text box."),
      h5("2. The predicted next word prints below it in blue."),
      h5("3. No need to hit enter of submit."),
      h5("4. A question mark means no prediction, typically due to mis-spelling"),
      h5("5. Additional tabs show plots of the top ngrams in the dataset"),
      br(),
      a("Source Code", href = "https://github.com/mark-blackmore/JHU-Data-Science-Capstone/tree/master/ngram_match")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("predict",
                 textInput("user_input", h3("Your Input:"), 
                           value = "Your words"),
                 h3("Predicted Next Word:"),
                 h4(em(span(textOutput("ngram_output"), style="color:blue")))),
        
        tabPanel("top quadgrams",
                 br(),
                 column(width = 12,
                        h3("Simulated Response Estimates"),
                        plotOutput(outputId = "quadgram_plot")),
                 introBox(numericInput(inputId = "QUAD_grams",
                                       20, label = "Number of Quad-grams"))
        ),
        
        tabPanel("top trigrams",
                 br(),       
                 column(width = 12,
                        h3("Simulated Response Estimates"),
                        plotOutput(outputId = "trigram_plot")),
                 introBox(numericInput(inputId = "TRI_grams",
                                       20, label = "Number of Tri-grams"))
        ),
        
        tabPanel("top bigrams",
                 br(),
                 column(width = 12,
                        h3("Simulated Response Estimates"),
                        plotOutput(outputId = "bigram_plot")),
                 introBox(numericInput(inputId = "BI_grams",
                                       20, label = "Number of Bi-grams"))
        )
      )   
    )
  )
)
#' Define server logic required to draw a histogram
server <- function(input, output) {
  
  dfFreq1 <-  readRDS("./app_data/dfFreq1.rds")
  dfFreq2 <-   readRDS("./app_data/dfFreq2.rds")
  dfFreq3  <- readRDS("./app_data/dfFreq3.rds")
  dfFreq4 <- readRDS("./app_data/dfFreq4.rds")
  
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
  
  output$quadgram_plot = renderPlot({
    options(repr.plot.width=8, repr.plot.height=3)
    ggplot(dfFreq4[1:input$QUAD_grams, ], aes(word, freq)) + 
      ggtitle("Quad-grams Frequencies") +
      geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
      coord_flip() + scale_y_continuous(name="Word Frequency") +
      scale_x_discrete(name="Quad-grams") +
      theme(plot.title =  element_text(face="bold", color="steelblue4",
                                       size=12),
            axis.text.x = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0))
  })
  
  output$trigram_plot = renderPlot({
    options(repr.plot.width=8, repr.plot.height=3)
    ggplot(dfFreq3[1:input$TRI_grams, ], aes(word, freq)) + 
      ggtitle("Tri-grams Frequencies") +
      geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
      coord_flip() + scale_y_continuous(name="Word Frequency") +
      scale_x_discrete(name="Tri-grams") +
      theme(plot.title =  element_text(face="bold", color="steelblue4",
                                       size=12),
            axis.text.x = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0))
  })
  
  output$bigram_plot = renderPlot({
    options(repr.plot.width=8, repr.plot.height=3)
    ggplot(dfFreq2[1:input$BI_grams, ], aes(word, freq)) + 
      ggtitle("Bi-grams Frequencies") +
      geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
      coord_flip() + scale_y_continuous(name="Word Frequency") +
      scale_x_discrete(name="Bi-grams") +
      theme(plot.title =  element_text(face="bold", color="steelblue4",
                                       size=12),
            axis.text.x = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0))
  })
  
}

#' Run the application 
shinyApp(ui = ui, server = server)