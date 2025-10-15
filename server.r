library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)


healthcare_dataset <- read_csv("healthcare_dataset.csv") # you have to copy these lines and then run in the console to be able to see them in the environment!
state_facts <- read_csv("state_facts_handcleaned.csv")


#function(input, output) {}



function(input,output){
  
  lats <- -90:90 #vector of all integers between the two
  lons <--180:180
  
  btn <- input$newButton # reevalutates input every time you click this button for new input data ... not used for anything, but makes this whole expression reevaluate so important!
  
  output$uSMap <- renderLeaflet({
    chlor
    leaflet() %>%
      setView(lng =sample(lons, 1),
              lat = sample(lats, 1),
              zoom = 5) %>%
      addTiles()    #to get map
    
  })
}


geo <- geojson_read("states.geo.json", what = "sp") # we will wait on this cloropleth for MONDAY!!!!

### chloropleth   - density gradient per state by specialty
### histogram?    - pipe for highest amounts of specialists by state (search a speciality, produces a list of states where that specialty is most dominant)
### in that histogram, colorcode by if specialty is 1 or 2 dominant for that state
#### x axis is specialty, y axis is total # (divide total # by the total for that state)




