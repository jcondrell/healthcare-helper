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
  mainPanel(
    plotOutput("main_plot")
  )
)

##################################


######################################
# THIS IS THE PIE CHART PER STATE!


##############################################
