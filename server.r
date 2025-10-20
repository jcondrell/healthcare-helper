library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)


healthcare_dataset <- read_csv("healthcare_dataset.csv") # you have to copy these lines and then run in the console to be able to see them in the environment!
state_facts <- read_csv("state_facts_handcleaned.csv")

specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)



# state_facts %>%
#   filter(SPECIALTY == input$)

################################
# 1 CHLOROPLETH:
# geo <- geojson_read("states.geo.json", what = "sp") # we will wait on this cloropleth for MONDAY!!!!
#######################################


######################################
# 2 THIS IS HISTOGRAM:
function(input, output) { #these function outputs are hard coded- they MUST say that
  
  output$main_plot <- renderPlot({ # all output functions are paired with a render function - make sure they match!!
   
     # filter data based on selected specialty
    filtered_data <- specialtyByState %>%
      filter(specialty == input$n_breaks)
    
    hist( state_facts$"specialty", #what makes the plot
          probability = TRUE, 
          breaks      = as.numeric(input$n_breaks), #important for making the graph dynamic/changeable on the website! this is where you are referring to the inputs
          xlab        = "States with Specialty Dominance",
          main        = "Specialty Prominance by State"
    )
    
    if (input$primary) { # if the thing inside this if statement is true, then this code will run
      rug(state_facts$"specialty")
    }
    
    if (input$primAndSec) {   # this responds to the check boxes on the webpage! when you check "Show density distribution," then this "if" statement runs
      dens <- density(state_facts$specialty)
      lines(dens, col = "red")
    }
    
  })
  
}

##################################





##############################
# THIS IS LEAFLT:
# function(input,output){
#   
#   lats <- -90:90 #vector of all integers between the two
#   lons <--180:180
#   
#   btn <- input$newButton # reevalutates input every time you click this button for new input data ... not used for anything, but makes this whole expression reevaluate so important!
#   
#   output$uSMap <- renderLeaflet({
#     chlor
#     leaflet() %>%
#       setView(lng =sample(lons, 1),
#               lat = sample(lats, 1),
#               zoom = 5) %>%
#       addTiles()    #to get map
#     
#   })
# }
###########################################





# MASTER PLAN:
### chloropleth   - density gradient per state by specialty
### histogram?    - pipe for highest amounts of specialists by state (search a speciality, produces a list of states where that specialty is most dominant)
### in that histogram, colorcode by if specialty is 1 or 2 dominant for that state
#### x axis is specialty, y axis is total # (divide total # by the total for that state)




