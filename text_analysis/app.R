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
library(visNetwork)
library(plotly)


ui <- dashboardPage(
  dashboardHeader(title="Text Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("list-alt")),
      menuItem("Token Variable and Cleaning", tabName = "to_know", icon = icon("table")),
      menuItem("Frequency Plots", tabName = "freq_plots", icon = icon("bar-chart-o")),
      menuItem("Sentiment Analysis", tabName = "sentiment_plots", icon = icon("bar-chart-o")),
      menuItem("Advanced Plots", tabName = "advanced_plots", icon = icon("bar-chart-o")),
      menuItem("Multiple Files", tabName = "multiple_files", icon = icon("list-alt"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction content
      tabItem(tabName = "introduction",
              fluidRow(box(
                title = "Introduction", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                textOutput("intro1"),
                br(),
                textOutput("intro2"),
                br(),
                textOutput("intro3"),
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
                           collapsible = TRUE, width = 12,
                           # got this from the help menu for fileInput
                           textOutput("dataIntro"),
                           br(),
                           fileInput("file1", label = NULL, multiple = TRUE,
                                     accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")),
                           textOutput("data1"),
                           radioButtons("file_type", "What type of file(s)?", 
                                        choices = c("CSV" = "csv", "TXT" = "txt")),
                           textOutput("data2"),
                           checkboxInput("multiple_files", tags$b("Are there multiple files?"), FALSE),
                           textOutput("data3"),
                           checkboxInput("header", tags$b("Are there variable names in the first line?"), TRUE),
                           textOutput("data4"),
                           checkboxInput("remove_stopwords", tags$b("Remove stop words"), TRUE),
                           radioButtons("disp", "How much raw data would you like to see?",
                                        choices = c('First few lines' = "head",
                                                    'Every line' = "all"),
                                        selected = "head"),
                           actionButton("submit", "Click here to display data"))),
              fluidRow(box(title="Raw Data", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("rawData"),
                           tableOutput("contents"))),
              fluidRow(box(title="Data Cleaning", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("dataCleaning"),
                           numericInput("start_line", "Line number to begin removal", value = 1),
                           numericInput("end_line", "Line number to end removal",
                                        value=1),
                           checkboxInput("no_removal", "I do not want to remove any lines of data", FALSE),
                           actionButton("update", "Update Data"))),
              fluidRow(box(title="Modified Data", status = "primary", solidHeader=TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("dataUpdate"),
                           tableOutput("result")))
      ),
      
      tabItem(tabName = "to_know",
              fluidRow(box(title = "Choose a token variable", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("tokenVar"),
                           br(),
                           selectInput("inSelect", label=NULL,
                                       c("Variable 1" = "option1",
                                         "Variable 2" = "option2")))),
              fluidRow(box(title = "Stop Words", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("stopWords"),
                           br(),
                           textInput("stopwords", "Enter words to remove here:"),
                           textOutput("removal")))
      ),
      
      # Frequency plots content
      tabItem(tabName = "freq_plots", 
              fluidRow(box(title = "What can frequency plots tell us about the text?", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, width = 8,
                           textOutput("freqMeaning"))),
              fluidRow(box(title = "Frequency Plot", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("freqDescription"),
                           plotlyOutput("freqPlot") %>% shinycssloaders::withSpinner(),
                           sliderInput("freq_count", "Change the minimum frequency count:", 
                                       min = 0, max = 500, value = 50))),
              fluidRow(box(title = "Wordcloud", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("wordcloudDescription"),
                           plotOutput("simple_wordcloud", width = "100%") %>% shinycssloaders::withSpinner(),
                           sliderInput("num_words", "Number of words in the cloud:", 
                                       min = 0, max = 100, value = 50)))
      ),
      
      # Sentiment analysis plots content
      tabItem(tabName = "sentiment_plots",
              fluidRow(box(title = "Overview", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("sentimentOverview"))),
              fluidRow(box(title = "Remove Sentiment Words", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("removeSentiments"),
                           textInput("sentimentwords", "Enter words to remove
                                     and separate each of them by a single space."))),
              fluidRow(box(title = "AFINN Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("afinnDescription"),
                           plotOutput("afinn_sentiment") %>% shinycssloaders::withSpinner(),
                           sliderInput("word_score", "Keep word scores with absolute value greater than:", 
                                       min = 0, max = 100, value = 25))),
              fluidRow(box(title = "Bing Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("bingDescription"),
                           plotOutput("bing_sentiment") %>% shinycssloaders::withSpinner())),
              fluidRow(box(title = "Sentiment Analysis Broken Down", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("chunkDescription"),
                           radioButtons("plottype", "How would you like to break your plot down?:",
                                        c("By groups of lines" = "bylines",
                                          "By chapter" = "bychapter")),
                           conditionalPanel(
                             condition = "input.plottype == 'bylines'",
                             sliderInput("breaks", "By how many lines per group?",
<<<<<<< HEAD
                               min = 0, max = 200, value = 100)),
                           plotOutput("byindex") %>% shinycssloaders::withSpinner())),
              fluidRow(box(title = "Wordcloud Colored by Sentiment", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
=======
                                         min = 0, max = 200, value = 100)),
                           plotOutput("byindex") %>% shinycssloaders::withSpinner()),
                       box(title = "Wordcloud Colored by Sentiment", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
>>>>>>> 0952447bd98836016ac90d1d179b0db342170227
                           textOutput("sentwcDescription"),
                           plotOutput("sentiment_wordcloud") %>% shinycssloaders::withSpinner(),
                           sliderInput("num_words2", "Number of words in the cloud:", 
                                       min = 0, max = 100, value = 50))),
              fluidRow(box(title = "Negated Sentiments", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("negateDescription"),
                           plotOutput("sentiment_negation") %>% shinycssloaders::withSpinner()))
                           ),
      
      # Advanced plots content
      tabItem(tabName = "advanced_plots",
              fluidRow(box(title = "Section Overview", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("advancedOverview"))),
              fluidRow(box(title = "Network Graph", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("networkDescription"),
                           plotOutput("network", #dblclick = "plot1_dblclick",
                                      brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE)) %>% 
                             shinycssloaders::withSpinner(),
                           sliderInput("cooccur", "Change the minimum number of cooccurrences:", 
                                       min = 0, max = 200, value = 5))),
                       #box(title = "Network 2", status = "primary", solidHeader = TRUE,
<<<<<<< HEAD
                        #   collapsible = TRUE, plotOutput("network2") %>% shinycssloaders::withSpinner()
                         #  ),
              fluidRow(box(title = "Co-occurrence Count", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
=======
                       #   collapsible = TRUE, plotOutput("network2") %>% shinycssloaders::withSpinner()
                       #  ),
                       box(title = "Co-occurrence Count", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
>>>>>>> 0952447bd98836016ac90d1d179b0db342170227
                           textOutput("countDescription"),
                           tableOutput("count_table") %>% shinycssloaders::withSpinner(),
                           sliderInput("byLines", "Number of lines:",
                                       min = 0, max = 10, value = 2))),
              fluidRow(box(title = "Correlation Tables", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("corrDescription"),
                           textInput("corr_words", "I want to see correlations with the word..."),
                           plotOutput("corr_comparison") %>% shinycssloaders::withSpinner())),
              fluidRow(box(title = "Correlation Network Graph", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           textOutput("corrnetworkDescription"),
                           plotOutput("corr_network") %>% shinycssloaders::withSpinner(),
                           sliderInput("corr", "Change the minimum correlation:", 
                                       min = 0, max = 1, value = 0.2)))
      ),
      
      tabItem(tabName = "multiple_files",
              textOutput("multipleDescription"),
              fluidRow(box(title = "Choose a faceting variable", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE,
                           textOutput("facetVar"),
                           br(),
                           selectInput("inSelectGroup", label=NULL,
                                       c("Variable 1" = "option1",
                                         "Variable 2" = "option2")))),
              fluidRow(box(title = "Frequency Plot", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           plotlyOutput("freqPlotGroup") %>% shinycssloaders::withSpinner(),
                           sliderInput("freq_count", "Change the minimum frequency count:", 
                                       min = 0, max = 500, value = 50)))
              
      )
)
  )
)


server <- function(input, output, session) {
  output$intro1 <- renderText("On a broad level, text analysis is the process of extracting information from 
                              complex text data. Text data can be anything from a speech to your favorite 
                              novel to a poet’s entire set of work. This module seeks to give the user (you!) 
                              an introduction to the basics of text analytics. We will uncover the most 
                              frequently used words, words with the strong emotional connotations, and 
                              meaningful relationships between words in your text.")
  output$intro2 <- renderText("This module is meant to serve as an introduction to the world of text analysis. 
                              We want to give people exposure to something they might not otherwise see. Text 
                              analytics can be intimidating and we want to make this introduction accessible 
                              to anyone who is interested.")
  output$intro3 <- renderText("We hope this module will be useful to undergraduates students enrolled in introductory 
                              English and Linguistics as well as students interested in Statistics and Data Science.")
  output$general1 <- renderText("THINGS TO THINK ABOUT:")
  output$general2 <- renderText("a. Twitter capabilities")
  output$general3 <- renderText("b. Web scraping - paste URL and it pulls text")
  output$general4 <- renderText("c. Additional, more specialized lexicons")
  output$general5 <- renderText("d. Separate data into files by chapter, date, etc (what we're doing now!)")
  output$general6 <- renderText("BUILT IN EXAMPLES:")
  output$general7 <- renderText("a. Peter Pan")
  output$general8 <- renderText("b. Amherst College Course Catalog")
  output$general9 <- renderText("c. Emily Dickinson Poems")
  
  
  output$dataIntro <- renderText("First off, we need to choose the file that contains the text data that we want to analyze.
                                 We can use .csv and .txt files to analyze our text data in the module. Please select a file from
                                 your computer than meets these requirements and then we can get started!")
  output$data1 <- renderText("Have a look at the extension of the file you chose - is it .csv or .txt? Knowing this will help
                             the program process your data.")
  output$data2 <- renderText("We can compare text across different pieces of text. If you have multiple files that 
                             you’d like to compare, please upload them above and check this box.")
  output$data3 <- renderText("Have a look at your file - are the names of the variables in the first line or 
                             does the text start right away?")
  output$data4 <- renderText("These are words we want to remove before processing the text. They aren’t integral to 
                             interpreting the main themes of the text and tend to be filler words like “and”, “the”, 
                             and “or”. Feel free to run the analysis with them removed (which is standard) and then 
                             with them left in and take a look at the different results you get!")
  
  data_set <- reactive({
    # got this code from this website:
    # https://itsalocke.com/blog/r-quick-tip-upload-multiple-files-in-shiny-and-consolidate-into-a-dataset/
    req(input$file1)
    
    
    if(input$file_type == "csv") {
      data_set <- as.data.frame(data.table::rbindlist(lapply(input$file1$datapath, data.table::fread),
                                                      use.names = TRUE, fill = TRUE))
    } else {
      path_list <- as.list(input$files$datapath)
      tbl_list <- lapply(input$files$datapath, read.table, header=TRUE, sep= " ")
      
      data_set <- do.call(rbind, tbl_list)
    }
  })
  
  
  observeEvent(
    input$submit, {
      output$rawData <- renderText("Have a look at the data. Does it look how you expected?")
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
  
  output$dataCleaning <- renderText("Sometimes there is part of the data that we don’t want to include in our analysis. F
                                    or instance, you might want to remove the table of contents. After looking at the raw 
                                    data, are there any lines that you would like to remove? If so, enter their line 
                                    numbers here.")
  
  new_data <- eventReactive(
    input$update, {
      output$dataUpdate <- renderText("Check out the modified data. If you removed some data, does it look better now? 
                                      If not, feel free to change which lines you removed up above.")
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
  
  output$tokenVar <- renderText(" In order to extract information from our text, we need to break it 
                                down into “pieces” that we care about. In this case, this means we want 
                                one word per line of data. The variable that contains this information 
                                is called the token variable -- please find the column that holds the 
                                text data we want to break down.")
  output$stopWords <- renderText("We mentioned stop words earlier on -- if there are words from your text that you don’t 
                                 want to have included in the analysis, please enter them here. If you'd like to remove
                                 multiple words, simply separate each one by a single white space.")
  
  plotdata <- reactive ({
    token <- new_data()[,input$inSelect]
    
    word_removal <- unlist(strsplit(input$stopwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    # Filter data based on whether or not there are stop words
    if(input$remove_stopwords == TRUE) {
      data_set <- cbind(new_data(), token) %>%
        mutate(text = as.character(token), linenumber=row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter ", 
                                                       ignore_case = TRUE)))) %>%
        unnest_tokens(word, text) %>%
        filter(word %nin% word_removal) %>%
        anti_join(stop_words)
    } else {
      data_set <- cbind(new_data(), token) %>%
        mutate(text = as.character(token), linenumber=row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter ", 
                                                       ignore_case = TRUE)))) %>%
        unnest_tokens(word, text) %>%
        filter(word %nin% word_removal)
    }
    
  })
  
  # Frequency plot
  output$freqDescription <- renderText("This plots displays the words that occur the most in your text. 
                                       Feel free to adjust slider to show words that occur more/less.")
  output$freqPlot <- renderPlotly ({
    g <- plotdata() %>%
      count(word) %>%
      filter(n > input$freq_count) %>% # will eventually want to make this a user input
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words in the Data') +
      ylab("Count") + xlab("Word") + theme(axis.text=element_text(size=12),
                                           axis.title=element_text(size=14,face="bold"),
                                           plot.title=element_text(size=16, face="bold"))
    
    ggplotly(g)
  })
  
  
  # Wordcloud for frequency of words (basically also just a frequency plot)
  output$wordcloudDescription <- renderText("This word cloud also shows the most common words. The larger the 
                                            word appears in the cloud, the more times it occurs in the text. 
                                            You can adjust how many words appear in the cloud with the slider below.")
  
  output$simple_wordcloud <- renderPlot ({
    plotdata() %>%
      count(word) %>%
      with(wordcloud::wordcloud(word, n, max.words = input$num_words, scale=c(4, 0.5), 
                                colors = RColorBrewer::brewer.pal(4, "Accent")))
  })
  
  output$freqMeaning <- renderText("Both of these plots give us a sense of the most commonly used words in the text. 
                                   These can be helpful in determining the overall topic and what might be most important 
                                   in the text.")
  # SENTIMENT ANALYSIS SECTION
  output$sentimentOverview <- renderText("In this section we will attempt to identify the words that contribute most to the 
                                         sentiment of this piece of text. We do this by attaching sentiment lexicons, which 
                                         are groups of words that have identified specific words and associated them with an 
                                         emotion. For example, this could be used to trace the plot of a book such as Peter 
                                         Pan or to identify the overall sentiment of a movie review.")
  output$removeSentiments <- renderText("Are there any words you'd like to manually remove from the text for sentiment analysis? 
                                        For instance, 'darling' has a positive sentiment but is also the family name in Peter 
                                        Pan, so we would want to remove it. Please enter your words here and separate them by 
                                        a single space. Note: you may want to revisit this step after having a look at the 
                                        sentiment analysis plots.")
  
  # AFINN Sentiment Graph
  output$afinnDescription <- renderText("This graph uses the AFINN sentiment lexicon that scores words on a scale from -5 to 5, 
                                        with -5 being most negative and 5 being most positive. That score is multiplied by the 
                                        number of times the word occurred and the overall word score is plotted on this graph. 
                                        Feel free to alter the minimum word score of words that appear on the graph by adjusting 
                                        the slider.")
  output$afinn_sentiment <- renderPlot({
    
    s_word_removal <- unlist(strsplit(input$sentimentwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    plotdata() %>%
      inner_join(get_sentiments("afinn")) %>%
      filter(word %nin% s_word_removal) %>%
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
    #  ggplotly(g1)
  })
  
  # Bing sentiment graph
  output$bingDescription <- renderText("The Bing sentiment lexicon categorizes words as either positive or negative. 
                                       This graph shows the “most positive” and “most negative” words according to this 
                                       specific lexicon. This is simply another way to do a simple sentiment analysis on 
                                       a chunk of text.")
  output$bing_sentiment <- renderPlot ({
    req(input$inSelect)
    
    s_word_removal <- unlist(strsplit(input$sentimentwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    plotdata() %>%
      inner_join(get_sentiments("bing")) %>%
      filter(word %nin% s_word_removal) %>%
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
  
  # Sentiment analysis by chunk/chapter of text
  output$chunkDescription <- renderText("This plot uses the AFINN lexicon and counts word score over a specified number of lines 
                                        or by chapter (it currently can only break down texts by chapter when the chapters literally
                                        begin with “Chapter”). When it shows blue bar above the middle line, that means that chunk 
                                        of text had more positively scored words than negatively scored words. In this sense, you can 
                                        track how the plot of a book changes by chapter.")
  output$byindex <- renderPlot ({
    s_word_removal <- unlist(strsplit(input$sentimentwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    if(input$plottype == "bylines") {
      plotdata() %>%
        inner_join(get_sentiments("afinn")) %>%
        filter(word %nin% s_word_removal) %>%
        group_by(index = linenumber %/% input$breaks) %>%
        summarise(sentiment = sum(score)) %>%
        ggplot(aes(index, sentiment, fill=sentiment > 0)) + 
        geom_col(show.legend = FALSE) + ggtitle("Sentiment Scores by Groups of Lines") +
        xlab("Index") + ylab("Sentiment Score") + theme(axis.text = element_text(size = 12), 
                                                        axis.title = element_text(size = 14,face = "bold"),
                                                        plot.title = element_text(size = 16, face = "bold")) 
    } else {
      plotdata() %>%
        inner_join(get_sentiments("afinn")) %>%
        filter(word %nin% s_word_removal) %>%
        group_by(chapter) %>%
        summarise(sentiment = sum(score)) %>%
        ggplot(aes(chapter, sentiment, fill=sentiment > 0)) + 
        geom_col(show.legend = FALSE) + ggtitle("Sentiment Scores by Chapter") +
        xlab("Chapter") + ylab("Sentiment Score") + theme(axis.text = element_text(size = 12), 
                                                          axis.title = element_text(size = 14,face = "bold"),
                                                          plot.title = element_text(size = 16, face = "bold"))
    }
  })
  
  # Sentiment wordcloud
  output$sentwcDescription <- renderText("This plot uses the positive and negative word categorizations from the Bing 
                                         sentiment lexicon. It shows the most positive words appear in large green 
                                         lettering and the most negative words appear in fuschia.")
  output$sentiment_wordcloud <- renderPlot ({
    req(input$inSelect)
    
    s_word_removal <- unlist(strsplit(input$sentimentwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    plotdata() %>%
      inner_join(get_sentiments("bing")) %>%
      filter(word %nin% s_word_removal) %>%
      count(word, sentiment, sort = TRUE) %>%
      reshape2::acast(word ~ sentiment, value.var = "n", fill=1) %>%
      wordcloud::comparison.cloud(colors = c("orchid", "seagreen2"), max.words = input$num_words2, 
                                  scale = c(4,0.5), title.size = 1)
  })
  
  # Data processing to get negated sentiment words
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
    s_word_removal <- unlist(strsplit(input$sentimentwords, split = " "))
    `%nin%` = Negate(`%in%`)
    
    clean_bigrams <- separate_bigrams() %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(word1 %nin% s_word_removal, word2 %nin% s_word_removal) %>%
      count(word1, word2, sort=TRUE)
  })
  
  # Not sure if I'll end up needing this one but I'll leave it for now
  united_bigrams <- reactive ({
    united_bigrams <- clean_bigrams() %>%
      unite(bigram, word1, word2, sep = " ")
  })
  
  # Negated sentiment words plot
  negation <- c("not", "no", "never", "without")
  
  output$negateDescription <- renderText("Although we are looking at the text one word at a time, we often need to consider 
                                         multiple words at once in order to get a better sense of what is going on in the 
                                         text. When you consider two words at a time, you can get a more nuanced sense of 
                                         what is going, which can be important for sentiment analysis. For instance, if you 
                                         consider the word ‘happy’ by itself, it is counted as a positive word. But if it is 
                                         preceded by the word ‘not’, then the phrase ‘not happy’ should be categorized as 
                                         negative. This plot displays the words that are most commonly preceded by negation 
                                         words (“not”, “no”, “none”, etc).")
  
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
  
  # ADVANCED PLOTS
  output$advancedOverview <- renderText("Correlation and co-occurrence show similar ideas -- they both contain information about 
                                        how often words occur together. Correlation is slightly more nuanced than co-occurrence in 
                                        that it also accounts for when words don’t occur together when calculating how strongly 
                                        related they are.")
  
  # Cooccurrence network graph
  output$networkDescription <- renderText("This network graph shows words that occur next to each other often. The darker the arrow 
                                          between them, the more times they co-occur. The arrows shows which order the words usually 
                                          go in. You can change the number of co-occurrences required for the pair to appear on the 
                                          graph. This gives a sense of words that commonly appear together. ")
  output$network <- renderPlot ({
    bigram_graph <- clean_bigrams() %>%
      filter(n > input$cooccur, !is.na(word1), !is.na(word2)) %>% # maybe want to make this a user input?
      igraph::graph_from_data_frame()
    
    
    ggraph::ggraph(bigram_graph, layout = "kk") + 
      ggraph::geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = arrow(length = unit(4, "mm")), 
                             start_cap = ggraph::circle(3, "mm"), end_cap = ggraph::circle(3, "mm")) + 
      ggraph::geom_node_point(color = "plum", size = 3) + 
      ggraph::geom_node_text(aes(label = name), hjust = 1.1, vjust = 1.1, size = 5) + 
      scale_edge_width(range = c(2,6)) + #scale_edge_colour_manual(color = "seagreen2") +
      theme_void()
  })
  
  
  output$network2 <- visNetwork::renderVisNetwork ({
    g1 <- clean_bigrams() %>% 
      filter(!is.na(word1)) %>% filter(!is.na(word2)) %>% filter(n > 3) %>% 
      graph_from_data_frame(directed = T)
    
    
    visIgraph(g1) %>%
      visNodes(size = 25, shape = "circle") %>%
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = TRUE) %>%
      visInteraction(keyboard = TRUE)
  })
  
  
  data_sections <- reactive ({
    token <- new_data()[,input$inSelect]
    sections <- cbind(new_data(), token) %>%
      mutate(section = row_number() %/% input$byLines, text = as.character(token)) %>% # by two lines - add option here
      filter(section > 0) %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word)
  })
  
  # top 10 word co-occurrences (taken for words that appear within the same two lines)
  output$countDescription <- renderText("This plots shows the words that occur most commonly together. By default, it counts
                                        words that are within two lines of each other.")
  output$count_table <- renderTable ({
    word_pairs <- data_sections() %>%
      pairwise_count(word, section, sort = TRUE)
    
    toDelete <- seq(1, nrow(word_pairs), 2)
    word_pairs <- word_pairs[toDelete,] %>%
      rename("First Word" = "item1", "Second Word" = "item2", "Count" = "n")
    
    head(word_pairs, 10)
  })
  
  word_cors <- reactive ({
    data_sections() %>%
      group_by(word) %>%
      filter(n() >= 20) %>% # will eventually want to make this value a user input
      pairwise_cor(word, section, sort = TRUE)
  })
  
  # Correlation tables
  output$corrDescription <- renderText("Oftentimes, we are interested in the relationships between words. One way we can approach 
                                       this idea is to look at correlations between words. If they have strong correlation, that 
                                       means they appear together a lot and also don’t appear often by themselves. Type in words 
                                       that you’re interested in seeing relationships with. Feel free to enter multiple words by 
                                       separating them by a single space. Correlations range between 0 and 1 with zero being no 
                                       association and 1 being fully strong.")
  output$corr_comparison <- renderPlot ({
    req(input$corr_words)
    
    correlation_words <- unlist(strsplit(input$corr_words, split = " "))
    
    word_cors() %>%
      filter(item1 %in% correlation_words) %>% 
      group_by(item1) %>%
      top_n(6) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation, fill=item1)) + 
      geom_bar(stat = "identity", show.legend=FALSE) + 
      facet_wrap(~item1, scales = "free_y") + coord_flip() + 
      ggtitle("Most Strongly Correlated Words") + 
      xlab("Correlation") + ylab("Word") +
      theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14,face = "bold"), 
            plot.title = element_text(size = 16, face = "bold"), strip.text.x = element_text(size = 12)) 
  })
  
  # Correlation Network
  output$corrnetworkDescription <- renderText("This network graph shows words that are strongly correlated with one another. A darker line between the two words 
                                              signifies a strong correlation. The label between the two words is their correlation (on a scale from 0 to 1).")
  output$corr_network <- renderPlot ({ # spatially doesn't look great right now
    word_cors() %>%
      filter(correlation > input$corr) %>% # make this changeable by user input
      graph_from_data_frame() %>%
      ggraph(layout = "fr") + geom_edge_link(aes(edge_alpha = correlation, 
                                                 label = round(correlation, 2)), show.legend = FALSE) + 
      geom_node_point(color = "plum", size = 3) + theme_void() + ggraph::geom_node_text(aes(label = name), hjust = 1.1, vjust = 1.1, size = 5) + 
      scale_edge_width(range = c(2,6))
  })
  
  output$multipleDescription <- renderText("This will be the area where we process multiple files. The structure for the data will be that all of the files
                                           get merged into one file and they have some file ID that distinguish which file they came from. Then you can basically 
                                           facet plots by the ID variable and get a plot for each file. For January work with Chris, I can add in tf-idf scores and
                                           other cool stuff that you can do when you have multiple data sources.")
  
  output$facetVar <- renderText("If you input multiple files, you may be wanting to look at graphs for each file, not just all of the files grouped together. In order
                                to create these graphs, we need to identify which variable holds the information that differentiates files from one another. This
                                will be some sort of ID variable.")
  
  observe({
    req(input$file1)
    updateSelectInput(session, "inSelectGroup",
                      label = "Choose a facet variable:",
                      choices = names(data_set()))
  })
  
  output$freqPlotGroup <- renderPlot ({
    plotdata() %>%
      count(word) %>%
      filter(n > input$freq_count) %>% # will eventually want to make this a user input
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words in the Data') +
      facet_grid(input$inSelectGroup ~ .) + ylab("Count") + xlab("Word") + theme(axis.text=element_text(size=12),
                                                                                 axis.title=element_text(size=14,face="bold"),
                                                                                 plot.title=element_text(size=16, face="bold"))
  })
  
}

shinyApp(ui = ui, server = server)

