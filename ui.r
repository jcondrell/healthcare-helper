library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)
library(leaflet)

healthcare_dataset <- read_csv("healthcare_dataset.csv")
state_facts <- read_csv("state_facts_handcleaned.csv")


################################
# 1 CHLOROPLETH:
  
###################################


######################################
# 2 THIS IS HISTOGRAM:


##################################





########################################
# THIS IS LEAFLT:
fluidPage(
  leafletOutput("uSMap"),
  br(),
  actionButton("newButton", "See which specialties are prominent in each state!")
)
########################################