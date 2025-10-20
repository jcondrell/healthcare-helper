library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)
library(leaflet)

healthcare_dataset <- read_csv("healthcare_dataset.csv")
state_facts <- read_csv("state_facts_handcleaned.csv")

specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)



################################
# 1 CHLOROPLETH:
  
###################################


######################################
# 2 THIS IS HISTOGRAM:


sidebarLayout( # see ?sidebarLayout for help on what this does
  sidebarPanel(  
    selectInput(inputId = "n_breaks",
                label = "Pick desired specialty:",
                choices = unique(specialtyByState$specialty),
                selected = c("Pick your desired specialty")
    ),
    checkboxInput(inputId = "primary",
                  label = strong("Show just the states with selected specialty as #1"), #putting the "strong" makes it bold
                  value = FALSE
    ), 
    checkboxInput(inputId = "primAndSec",
                  label = strong("Show the states that have selected specialty as their #1 and #2 (will give more results)"),
                  value = FALSE
    ),
  ), 
  mainPanel(
    plotOutput("main_plot")
  )
  
  
)

##################################





########################################
# THIS IS LEAFLT:
# fluidPage(
#   leafletOutput("uSMap"),
#   br(),
#   actionButton("newButton", "See which specialties are prominent in each state!")
# )
########################################