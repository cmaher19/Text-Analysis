#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidytext)
library(tidyverse)
library(gutenbergr)
library(urltools)
library(shinythemes)

# Peter Pan example from Gutenberg - create a line number variable, tokenize the data, and remove stop words
#peterpan <- gutenberg_download(16) 

peterpan <- peterpan %>%
  mutate(linenumber=row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Might want to display raw data - offer option for removing lines that don't contain the actual text?

ui <- dashboardPage(
  dashboardHeader(title="Text Analysis"),
  dashboardSidebar(h4(textOutput("intro")),
                   textOutput("thought1"),
                   textOutput("thought2")),
  dashboardBody(
    fluidRow(
      box(
        fileInput("file1", "Choose File", accept = c("text/csv", "text/comma-separated-values,
                                                         text/plain", ".csv"))),
      box(plotOutput("plot1")),
      box(plotOutput("plot2")),
      box(plotOutput("plot3")),
      box(plotOutput("plot4")),
      box(plotOutput("plot5"))
    )
  )
)


server <- function(input, output) {
  output$intro <- renderText("For now, I'm just going to keep notes here as I go (eventually this will be
                             an introduction to the interface and text analysis).")
  output$thought1 <- renderText("Do we want to display the raw data at first and then allow the user to remove
                                lines that are metadata/useless/whatever?")
  output$thought2 <- renderText("Need to put description for graphs as well as update graph labels")
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })
  output$plot1 <- renderPlot({
    peterpan %>%
      count(word) %>%
      filter(n > 50) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words in Peter Pan') +
      ylab("Count") + xlab("Word")
  })
  # Words with the largest contribution according to AFINN lexicon
  output$plot2 <- renderPlot({
    peterpan %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(score) %>%
      count(word, sort=TRUE) %>%
      ungroup() %>%
      mutate(word_score = score*n) %>%
      filter(abs(word_score) > 25) %>%
      mutate(word = reorder(word, word_score)) %>%
      ggplot(aes(word, n*score, fill=n*score>0)) + geom_col(show.legend = FALSE) + 
      coord_flip() + ggtitle("Words with largest Sentiment Contribution")
  })
  # Most commonly occurring positive and negative words according to the bing lexicon
  output$plot3 <- renderPlot({
    peterpan %>%
      inner_join(get_sentiments("bing")) %>%
      filter(word != "darling") %>%
      group_by(sentiment) %>%
      count(word, sort=TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) + geom_col(show.legend = FALSE) + 
      facet_wrap(~sentiment, scales="free_y") + coord_flip() + 
      ggtitle('Most Common Sentiments with Bing Lexicon')
  })
  # Break book down by chunks of 75 lines and analyze sentiments that way
  output$plot4 <- renderPlot({
    peterpan %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(index = linenumber %/% 75) %>%
      summarise(sentiment = sum(score)) %>%
      ggplot(aes(index, sentiment, fill=sentiment > 0)) + geom_col(show.legend = FALSE) + 
      ggtitle("Sentiment Scores every 75 Lines")
  })
  output$plot5 <- renderPlot({
    pp_afinn <- peterpan %>%
      inner_join(get_sentiments("afinn")) %>%
      group_by(index = linenumber %/% 75) %>%
      summarise(sentiment = sum(score)) %>%
      mutate(method = "AFINN")
    
    pp_bing <- peterpan %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(method = "Bing") %>%
      count(method, index = linenumber %/% 75, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    # The lexicons show roughly the same trends
    # Note: remove darling from afinn list
    bind_rows(pp_afinn, pp_bing) %>%
      ggplot(aes(index, sentiment, fill=method)) + geom_col(show.legend = FALSE) + 
      facet_wrap(~method, ncol = 1, scales = "free_y") + ggtitle("Comparing Sentiment Lexicons for Peter Pan")
  })
}

shinyApp(ui = ui, server = server)

