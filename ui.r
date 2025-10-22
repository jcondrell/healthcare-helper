library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)
library(leaflet)

healthcare_dataset <- read_csv("healthcare_dataset.csv")
state_facts <- read_csv("state_facts_handcleaned.csv")

specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)


ui <- navbarPage("Healthcare Helper",
                 
         # Really wanted to change all the fonts to Times New Roman... :
         tags$head(
           tags$style(HTML("
            body {
              font-family: 'Times New Roman', Times, serif;
             }
          "))
         ),
         
                 
        # Creating the tabs and the main header page:
           tabPanel("Home",
                    h2("Welcome to the healthcare helper app!"),
                    p("We are here to help you find accesible healthcare data to better help you assess your healthcare needs and options!")
           ),
           
  
################################
# 1 CHLOROPLETH:
  
###################################


######################################
# 2 THIS IS HISTOGRAM:
tabPanel("Find most prominent specialties by state",
         
  sidebarLayout(
    sidebarPanel(  
      selectInput(inputId = "n_breaks",
                  label = "Pick desired specialty:",
                  choices = unique(specialtyByState$specialty),
                  selected = c("Pick your desired specialty")
      ),
      checkboxInput(inputId = "show_percent",
                    label = strong("Show percentage of state's total physicians"),
                    value = FALSE
      )
    ),
    
    # this makes the heading within the tab:
    mainPanel(
      h3("Understanding Specialty Distribution"),
      p("This chart can help you find the top 10 states with the most physicians in your selected specialty. 
        States highlighted in red indicate where this specialty is the #1 most common specialty 
        in that state, while blue indicates that there is still a high quantity of physicians in that specialty, but it may not be #1 in that state."),
      plotOutput("main_plot")
    )
  )
  
),
##################################


######################################
# 3 THIS IS THE PIE CHART PER STATE!
tabPanel("State by state pie chart specialty analysis",
         
  selectInput("state_select",
              "Select a State:",
              choices = unique(specialtyByState$Location),
              selected = unique(specialtyByState$Location)[1]),
  mainPanel(
    h3("Understanding specialty percentages within selected states:"),
    p("This pie chart allows you to select a state and see which specialties are most dominant in that state."),
  
  plotOutput("pie_chart")
  )

)
##############################################


) # connects to navbarPage at top! must engulf WHOLE THING!
