library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)


# healthcare_dataset <- read_csv("healthcare_dataset.csv")
# state_facts <- read_csv("state_facts.csv")

data <- readRDS("NEW_datasets.RDS")
healthcare_dataset <- data$healthcare_dataset
state_facts <- data$state_facts

#function(input, output) {}



function(input,output){
  
  lats <- -90:90 #vector of all integers between the two
  lons <--180:180
  
  btn <- input$newButton # reevalutates input every time you click this button for new input data ... not used for anything, but makes this whole expression reevaluate so important!
  
  output$uSMap <- renderLeaflet({
    
    leaflet() %>%
      setView(lng =sample(lons, 1),
              lat = sample(lats, 1),
              zoom = 5) %>%
      addTiles()    #to get map
    
  })
}