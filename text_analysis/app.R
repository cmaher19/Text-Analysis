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

# Might want to display raw data - offer option for removing lines that don't contain the actual text?

ui <- dashboardPage(
  dashboardHeader(title="Text Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Data Upload", tabName = "data_upload", icon = icon("th")),
      menuItem("Plots", tabName = "plots", icon = icon("dashboard"))
    )
  ),
  
    dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              box(
                title = "Introduction", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                box(textOutput("thought1")),
                box(textOutput("thought2"))
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "data_upload",
              fluidRow(box(
                # got this from the help menu for fileInput
                fileInput("file1", "Please Choose a CSV/Text File", accept = c("text/csv", "text/comma-separated-values,
                                                                           text/plain", ".csv")),
                radioButtons("disp", "How much raw data would you like to see?",
                             choices = c('First few lines' = "head",
                                         'Every line' = "all"),
                             selected = "head")),
                box(title="Are there variables in the first line?", checkboxInput('header', 'Header', TRUE)),
                box(selectInput("inSelect",
                                "Choose a tokenizer variable:",
                                c("label 1" = "option1",
                                  "label 2" = "option2")))
                ),
              box(title="Raw Data", tableOutput("contents"))
              
              ),
      tabItem(tabName="plots",
              fluidRow(box(plotOutput("freqPlot")))
      )
      )
    )
)


server <- function(input, output, session) {
  output$intro <- renderText("For now, I'm just going to keep notes here as I go (eventually this will be
                             an introduction to the interface and text analysis).")
  output$thought1 <- renderText("Do we want to display the raw data at first and then allow the user to remove
                                lines that are metadata/useless/whatever?")
  output$thought2 <- renderText("Need to put description for graphs as well as update graph labels")
  
  dsnames <- c()
  data_set <- reactive({
    req(input$file1)
    inFile <- input$file1
    data_set<-read.csv(inFile$datapath, header=input$header)
  })
  
  output$contents <- renderTable({
    if(input$disp == "head") {
      return(head(data_set()))
    }
    else {
      return(data_set())
    }
  })
  
  observe({
    req(input$file1)
    dsnames <- names(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateSelectInput(session, "inSelect",
                             label = "Variables",
                             choices = cb_options,
                             selected = "")
  })
  
  output$freqPlot <- renderPlot({
    data_set() %>%
      mutate(text = as.character(text)) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word) %>%
      filter(n > 50) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) + geom_col(fill="purple") + coord_flip() + ggtitle('Most Common Words') +
      ylab("Count") + xlab("Word")
  })

}

shinyApp(ui = ui, server = server)

