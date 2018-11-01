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
library(shinythemes)
library(tidytext)
library(tidyverse)
library(widyr)
library(igraph)
library(ggraph)


ui <- dashboardPage(
  dashboardHeader(title="Text Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("th")),
      menuItem("Things to Know", tabName = "to_know", icon = icon("th")),
      menuItem("Frequency Plots", tabName = "freq_plots", icon = icon("dashboard")),
      menuItem("Sentiment Analysis", tabName = "sentiment_plots", icon = icon("dashboard")),
      menuItem("Advanced Plots", tabName = "advanced_plots", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction content
      tabItem(tabName = "introduction",
              fluidRow(box(
                title = "Introduction", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                textOutput("description"),
                br(),
                textOutput("intro"),
                br(),
                textOutput("thought1"),
                br(),
                textOutput("thought2"),
                br(),
                textOutput("thought3"),
                br(),
                textOutput("thought4"),
                br(),
                textOutput("thought5"),
                br(),
                textOutput("thought6"),
                br(),
                textOutput("thought7"),
                br(),
                textOutput("thought8"),
                br()
              ),
              box(title = "General Project Thoughts", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("general1"),
                  br(),
                  textOutput("general2"),
                  br(),
                  textOutput("general3"),
                  br(),
                  textOutput("general4"),
                  br(), 
                  textOutput("general5"),
                  br(),
                  textOutput("general6"),
                  br(),
                  textOutput("general7"),
                  br(),
                  textOutput("general8"),
                  br(),
                  textOutput("general9")
              ))
              
      ),
      
      # Data upload content
      tabItem(tabName = "data_upload",
              fluidRow(box(title="Choose a CSV/Text File", status = "primary", solidHeader =TRUE,
                           collapsible = TRUE,
                           # got this from the help menu for fileInput
                           # ADD OPTION TO TAKE IN MULTIPLE FILES
                           fileInput("file1", label = NULL, multiple = TRUE,
                                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                           checkboxInput('header', 'Check box if there are variable names in the first line.', TRUE),
                           radioButtons("disp", "How much raw data would you like to see?",
                                        choices = c('First few lines' = "head",
                                                    'Every line' = "all"),
                                        selected = "head"),
                           actionButton("submit", "Submit")),
                       box(title="Raw Data", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, tableOutput("contents"))),
              fluidRow(box(title="Data Cleaning", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, numericInput("start_line", "Line number to begin removal", 
                                                            value = 1),
                           numericInput("end_line", "Line number to end removal",
                                        value=1),
                           checkboxInput("no_removal", "I do not want to remove any lines of data", FALSE),
                           actionButton("update", "Update Data")),
                       box(title="Modified Data", status = "primary", solidHeader=TRUE,
                           collapsible = TRUE, tableOutput("result")))
      ),
      
      tabItem(tabName = "to_know",
              fluidRow(box(title = "Choose a token variable", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, 
                           selectInput("inSelect", label=NULL,
                                       c("Variable 1" = "option1",
                                         "Variable 2" = "option2")))),
              fluidRow(box(title = "Stop Words", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textInput("stopwords", "Are there any words you'd like to manually remove from the text?
                                     Please enter them here.")))
      ),
      
      # Frequency plots content
      tabItem(tabName = "freq_plots", 
              fluidRow(box(title = "Frequency Plot", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("freqPlot"),
                           sliderInput("freq_count", "Change the minimum frequency count:", 
                                       min = 0, max = 500, value = 50)),
                       box(title = "Wordcloud", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, 
                           plotOutput("simple_wordcloud"),
                           sliderInput("num_words", "Number of words in the cloud:", 
                                       min = 0, max = 200, value = 100)))
      ),
      
      # Sentiment analysis plots content
      tabItem(tabName = "sentiment_plots",
              fluidRow(box(title = "Remove Sentiment Words", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textInput("sentimentwords", "Are there any words you'd like to manually remove from the text
                                      for sentiment analysis? For instance, 'darling' has a positive sentiment but is also
                                      the family name in Peter Pan, so we would want to remove it. Please enter your words
                                      here."))),
              fluidRow(box(title = "AFINN Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("afinn_sentiment"),
                           sliderInput("word_score", "Keep word scores with absolute value greater than:", 
                                       min = 0, max = 100, value = 25)),
                       box(title = "Bing Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("bing_sentiment"))),
              fluidRow(box(title = "Sentiment Analysis Broken Down", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("bylinenumber"),
                           sliderInput("chunk_size", "How many lines would you like in each group?", 
                                       min = 0, max = 200, value = 100)),
                       box(title = "Wordcloud Colored by Sentiment", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("sentiment_wordcloud"),
                           sliderInput("num_words2", "Number of words in the cloud:", 
                                       min = 0, max = 200, value = 100))),
              fluidRow(box(title = "Negated Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("sentiment_negation")))
      ),
      
      # Advanced plots content
      tabItem(tabName = "advanced_plots",
              fluidRow(box(title = "Network Graph", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("network"),
                           sliderInput("cooccur", "Change the minimum number of cooccurrences:", 
                                       min = 0, max = 200, value = 10)),
                       box(title = "Co-occurrence Count", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           tableOutput("count_table"))),
              fluidRow(box(title = "Correlation Tables", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textInput("corr_words", "I want to see correlations with the word..."),
                           plotOutput("corr_comparison")),
                       box(title = "Correlation Network Graph", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("corr_network"),
                           sliderInput("corr", "Change the minimum correlation:", 
                                       min = 0, max = 1, value = 0.2)))
      )
    )
  )
)


server <- function(input, output, session) {
  output$intro <- renderText("TO DO:")
  output$thought1 <- renderText("1. Add something where user can decide count cutoff in frequency plots, 
                                sentiment plots, word clouds, etc. For instance, you can input that you only want to show 
                                word with a sentiment score > 10 or that you want to break your file down by
                                chunks of 50 lines and look at it that way.")
  output$thought2 <- renderText("2. Only outputting head of modified data - add ability to see it all.")
  output$thought3 <- renderText("3. Figure out how to add index to txt files - project gutenberg stuff does it 
                                automatically and that's all I've been testing on, so it has worked")
  output$thought4 <- renderText("4. Add descriptions/explanations for what various things are doing and why
                                they are important for text analysis.")
  output$thought5 <- renderText("5. Figure out how to allow user to upload multiple files and store them properly.
                                This allows us to use multiple (e.g. where one files is one chapter of a book) files
                                and make a corpus that can then be analyzed.")
  output$thought6 <- renderText("6. Make new tabs for different topics (e.g. could have a tab for sentiment
                                analysis).")
  output$thought7 <- renderText("7. Allow user to remove specific words from stop word list or sentiment analysis.
                                For example, Chris and I found that 'darling' has a positive sentiment, but
                                is just the family name in Peter Pan.")
  output$thought8 <- renderText("8. Should explanations automatically come up or should they be optional
                                via only appearing when you hover over a plot or something?")
  output$description <- renderText("This will eventually be an introductory description of the interface.")
  output$general1 <- renderText("THINGS TO THINK ABOUT:")
  output$general2 <- renderText("a. Twitter capabilities")
  output$general3 <- renderText("b. Web scraping - paste URL and it pulls text")
  output$general4 <- renderText("c. Additional, more specialized lexicons")
  output$general5 <- renderText("d. Separate data into files by chapter, date, etc (what we're doing now!)")
  output$general6 <- renderText("BUILT IN EXAMPLES:")
  output$general7 <- renderText("a. Peter Pan")
  output$general8 <- renderText("b. Amherst College Course Catalog")
  output$general9 <- renderText("c. Emily Dickinson Poems")
  
  data_set <- reactive({
    # got this code from this website:
    # https://itsalocke.com/blog/r-quick-tip-upload-multiple-files-in-shiny-and-consolidate-into-a-dataset/
    req(input$file1)
    data_set <- as.data.frame(rbindlist(lapply(input$file1$datapath, fread),
              use.names = TRUE, fill = TRUE))
    
    #data_set<-read.csv(input$file1$datapath, header=input$header)
  })
  
  observeEvent(
    input$submit, {
      output$contents <- renderTable({
        if(input$disp == "head") {
          return(head(data_set()))
        }
        else {
          return(data_set())
        }
      })
    } 
  )
  
  new_data <- eventReactive(
    input$update, {
      if(input$no_removal == FALSE) {
        data_set()[-c(input$start_line:input$end_line), ]
      }
      else {
        data_set()
      }
    }
  )
  # only shows head of modified data - may want to make you able to see all
  output$result <- renderTable(head({new_data()}))
  
  observe({
    req(input$file1)
    updateSelectInput(session, "inSelect",
                      label = "Choose a token variable:",
                      choices = names(data_set()))
  })
  
  
  plotdata <- reactive ({
    token <- new_data()[,input$inSelect]
    data_set <- cbind(new_data(), token) %>%
      mutate(text = as.character(token), linenumber=row_number()) %>%
      unnest_tokens(word, text) %>%
      filter(word != input$stopwords) %>%
      anti_join(stop_words)
  })
  
  output$freqPlot <- renderPlot ({
    plotdata() %>%
      count(word) %>%
      filter(n > input$freq_count) %>% # will eventually want to make this a user input
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words in the Data') +
      ylab("Count") + xlab("Word") + theme(axis.text=element_text(size=12),
                                           axis.title=element_text(size=14,face="bold"),
                                           plot.title=element_text(size=16, face="bold"))
  })
  
  output$simple_wordcloud <- renderPlot ({
    plotdata() %>%
      count(word) %>%
      with(wordcloud::wordcloud(word, n, max.words = input$num_words, scale=c(4, 0.5), 
                                colors = RColorBrewer::brewer.pal(4, "Accent")))
  })
  
  output$afinn_sentiment <- renderPlot({
    plotdata() %>%
      inner_join(get_sentiments("afinn")) %>%
      filter(word != input$sentimentwords) %>%
      group_by(score) %>%
      count(word, sort=TRUE) %>%
      ungroup() %>%
      mutate(word_score = score*n) %>%
      filter(abs(word_score) > input$word_score) %>% # will eventually want to make this a user input
      mutate(word = reorder(word, word_score)) %>%
      ggplot(aes(word, n*score, fill=n*score>0)) + geom_col(show.legend = FALSE) + 
      coord_flip() + ggtitle("Words with largest Sentiment Contribution from AFINN Lexicon") +
      ylab("Word Score") + xlab("Word") + theme(axis.text=element_text(size=12),
                                                axis.title=element_text(size=14,face="bold"),
                                                plot.title=element_text(size=14, face="bold"))
  })
  
  output$bing_sentiment <- renderPlot ({
    req(input$inSelect)
    
    plotdata() %>%
      inner_join(get_sentiments("bing")) %>%
      filter(word != input$sentimentwords) %>%
      group_by(sentiment) %>%
      count(word, sort=TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) + geom_col(show.legend = FALSE) + 
      facet_wrap(~sentiment, scales="free_y") + coord_flip() + 
      ggtitle('Most Common Sentiments with Bing Lexicon') + ylab("Count") + xlab("Word") +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14,face = "bold"),
            plot.title = element_text(size = 16, face = "bold"), 
            strip.text.x = element_text(size = 12, face = "bold")) 
  })
  
  output$bylinenumber <- renderPlot ({
    plotdata() %>%
      inner_join(get_sentiments("afinn")) %>%
      filter(word != input$sentimentwords) %>%
      group_by(index = linenumber %/% input$chunk_size) %>%
      summarise(sentiment = sum(score)) %>%
      ggplot(aes(index, sentiment, fill=sentiment > 0)) + 
      geom_col(show.legend = FALSE) + ggtitle("Sentiment Scores by Chunks of Text") +
      xlab("Index") + ylab("Sentiment Score") + theme(axis.text = element_text(size = 12), 
                                                      axis.title = element_text(size = 14,face = "bold"),
                                                      plot.title = element_text(size = 16, face = "bold")) 
  })
  
  output$sentiment_wordcloud <- renderPlot ({
    req(input$inSelect)
    
    plotdata() %>%
      inner_join(get_sentiments("bing")) %>%
      filter(word != input$sentimentwords) %>%
      count(word, sentiment, sort = TRUE) %>%
      reshape2::acast(word ~ sentiment, value.var = "n", fill=1) %>%
      wordcloud::comparison.cloud(colors = c("orchid", "seagreen2"), max.words = input$num_words2, 
                                  scale = c(4,0.5), title.size = 1)
  })
  
  
  bigrams <- reactive ({
    token <- new_data()[,input$inSelect]
    bigram_data <- cbind(new_data(), token) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2) 
  })
  
  separate_bigrams <- reactive ({
    separate_bigrams <- bigrams() %>%
      separate(bigram, c("word1", "word2"), sep = " ")
  })
  
  clean_bigrams <- reactive ({
    clean_bigrams <- separate_bigrams() %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      count(word1, word2, sort=TRUE)
  })
  
  # Not sure if I'll end up needing this one but I'll leave it for now
  united_bigrams <- reactive ({
    united_bigrams <- clean_bigrams() %>%
      unite(bigram, word1, word2, sep = " ")
  })
  
  
  negation <- c("not", "no", "never", "without")
  
  output$sentiment_negation <- renderPlot ({
    negation_words <- separate_bigrams() %>%
      filter(word1 %in% negation) %>%
      inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
      count(word2, score, sort = TRUE) %>%
      ungroup()
    
    negation_words %>%
      mutate(contribution = n*score) %>%
      arrange(desc(abs(contribution))) %>%
      head(20) %>%
      mutate(word2 = reorder(word2, contribution)) %>%
      ggplot(aes(word2, n*score, fill = n*score > 0)) + 
      geom_col(show.legend = FALSE) + xlab("Words preceded by a negation word") + 
      ylab("Sentiment score * number of occurrences") + coord_flip() + 
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14,face = "bold"),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$network <- renderPlot ({
    bigram_graph <- clean_bigrams() %>%
      filter(n > input$cooccur) %>% # maybe want to make this a user input?
      igraph::graph_from_data_frame()
    
    # Create arrow
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    ggraph::ggraph(bigram_graph, layout = "fr") + 
      ggraph::geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, 
                             end_cap = ggraph::circle(.07, "inches")) + 
      ggraph::geom_node_point(color = "plum", size = 3) + 
      ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
      theme_void()
  }) 
  
  
  data_sections <- reactive ({
    token <- new_data()[,input$inSelect]
    sections <- cbind(new_data(), token) %>%
      mutate(section = row_number() %/% 2, text = as.character(token)) %>%
      filter(section > 0) %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word)
  })
  
  # top 10 word co-occurrences (taken for words that appear within the same two lines)
  output$count_table <- renderTable ({
    word_pairs <- data_sections() %>%
      pairwise_count(word, section, sort = TRUE)
    
    toDelete <- seq(1, nrow(word_pairs), 2)
    word_pairs <- word_pairs[toDelete,] %>%
      rename("Word 1" = "item1", "Word 2" = "item2", "Count" = "n")
    
    head(word_pairs, 10)
  })
  
  word_cors <- reactive ({
    data_sections() %>%
      group_by(word) %>%
      filter(n() >= 20) %>% # will eventually want to make this value a user input
      pairwise_cor(word, section, sort = TRUE)
  })
  
  output$corr_comparison <- renderPlot ({ # this will just be a placeholder for now until I get words inputs going
    req(input$corr_words)
    
    word_cors() %>%
      filter(item1 == input$corr_words) %>% # this is where input will go
      group_by(item1) %>%
      top_n(6) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation, fill=item1)) + 
      geom_bar(stat = "identity", show.legend=FALSE) + 
      facet_wrap(~item1, scales = "free_y") + coord_flip() + 
      ggtitle("Most Strongly Correlated Words")
  })
  
  
  output$corr_network <- renderPlot ({ # spatially doesn't look great right now
    word_cors() %>%
      filter(correlation > input$corr) %>% # make this changeable by user input
      graph_from_data_frame() %>%
      ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = correlation, label = round(correlation, 2)), show.legend = FALSE) + 
      geom_node_point(color = "plum", size = 3) + geom_node_text(aes(label = name), repel = TRUE) + 
      theme_void()
  })
  
}

shinyApp(ui = ui, server = server)

