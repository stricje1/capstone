#Shiny App

#This script creates a Shiny App that takes a word or phrase input in a text box and outputs the a predicted next word.

library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(ngram)
})

#Source ngram matching function

source("ngram.R")

#Define UI for application that draws a histogram

ui <- fluidPage(
  
# Application title
  titlePanel("Text Prediction Model"), p("This app that takes an input phrase (multiple words) in a text box and outputs a prediction of the next word."),
  
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
      a("Source Code", href = "https://github.com/stricje1/capstone")
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
                 fluidRow(
                   hr(), 
                   column(width = 12,
                          h3("Simulated Response Estimates"),
                          plotOutput(outputId = "quadgram_plot"))
                   )),
        
        tabPanel("top trigrams",
                 br(),       
                 img(src = "trigrams.png", height = 500, width = 700)),
        
        tabPanel("top bigrams",
                 br(),
                 img(src = "bigrams.png", height = 500, width = 700)),
        )   
    )
  )
)


#Define server logic required to draw a histogram

server <- function(input, output) {

  
  output$ngram_output <- renderText({
    ngram(input$user_input)
  })
  
  output$quadgram_plot = renderPlot({
    freq4 <- rowSums(as.matrix(dtm4))
    freq4 <- sort(freq4, decreasing = TRUE)
    dfFreq4 <- data.frame(word = names(freq4), freq=freq4)
    options(repr.plot.width=8, repr.plot.height=3)
    ggplot(dfFreq4[1:20, ], aes(word, freq)) + 
      ggtitle("4-grams Frequencies") +
      geom_bar(stat = "identity", fill="dodgerblue", colour="skyblue") +
      coord_flip() + scale_y_continuous(name="Word Frequency") +
      scale_x_discrete(name="1-grams") +
      theme(plot.title =  element_text(face="bold", color="steelblue4",
                                       size=12),
            axis.text.x = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0),
            axis.text.y = element_text(face="bold", color="steelblue4",
                                       size=8, angle=0))
  })
}

#Run the application

shinyApp(ui = ui, server = server)

#Shiny applications not supported in static R Markdown documents