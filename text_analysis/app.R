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
      menuItem("Data Upload", tabName = "data_upload", icon = icon("th"))
    )
  ),
  
    dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              box(textOutput("thought1"),
              textOutput("thought2"))
      ),
      
      # Second tab content
      tabItem(tabName = "data_upload",
              fluidRow(box(
                # got this from the help menu for fileInput
                fileInput("file1", "Please Choose a Text File", accept = c("text/csv", "text/comma-separated-values,
                                                                           text/plain", ".csv")),
                radioButtons("disp", "Display",
                             choices = c('First few lines' = "head",
                                         'Every line' = "all"),
                             selected = "head")
                )),
              box(tableOutput("contents")),
              fluidRow(
                box(checkboxInput('header', 'Header', TRUE)),
                box(checkboxGroupInput("inCheckboxGroup",
                                       "Checkbox group input:",
                                       c("label 1" = "option1",
                                         "label 2" = "option2"))),
                box(radioButtons('sep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ',')),
                box(radioButtons('quote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"')),
                box(uiOutput("choose_columns"))
              )
      ))

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
    data_set<-read.csv(inFile$datapath, header=input$header, 
                       sep=input$sep, quote=input$quote)
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
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Check Box Group",
                             choices = cb_options,
                             selected = "")
  })
  
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    
    colnames <- names(contents)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
}

shinyApp(ui = ui, server = server)

