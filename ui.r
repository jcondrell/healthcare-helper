library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)
library(leaflet)

# healthcare_dataset <- read_csv("healthcare_dataset.csv")
# state_facts <- read_csv("state_facts.csv")

data <- readRDS("NEW_datasets.RDS")
healthcare_dataset <- data$healthcare_dataset
state_facts <- data$state_facts



fluidPage(
  leafletOutput("uSMap"),
  br(),
  actionButton("newButton", "See which specialties are prominent in each state!")
)