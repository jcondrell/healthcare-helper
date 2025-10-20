library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)
library(dplyr)



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
function(input, output) {
  
  output$main_plot <- renderPlot({
    
    # Calculate total physicians per state
    state_totals <- specialtyByState %>%
      filter(Location != "United States") %>%
      group_by(Location) %>%
      summarise(total_physicians = sum(physicianNumbers))
    
    # Find which specialty is #1 for each state
    top_specialty_by_state <- specialtyByState %>%
      filter(Location != "United States") %>%
      group_by(Location) %>%
      filter(physicianNumbers == max(physicianNumbers)) %>%
      select(Location, top_specialty = specialty) %>%
      ungroup()
    
    # Get top 10 states for selected specialty
    plot_data <- specialtyByState %>%
      filter(specialty == input$n_breaks, Location != "United States") %>%
      arrange(desc(physicianNumbers)) %>%
      slice(1:10) %>%
      left_join(top_specialty_by_state, by = "Location") %>%
      left_join(state_totals, by = "Location") %>%
      mutate(is_top = ifelse(specialty == top_specialty, "Top Specialty", "Not Top"),
             percentage = round((physicianNumbers / total_physicians) * 100, 1))
    
    # Create bar plot
    p <- ggplot(plot_data, aes(x = reorder(Location, -physicianNumbers), 
                               y = physicianNumbers, 
                               fill = is_top)) +
      geom_col() +
      scale_fill_manual(values = c("Top Specialty" = "firebrick1", "Not Top" = "dodgerblue")) +
      labs(title = paste("Top 10 States for", input$n_breaks),
           x = "State",
           y = "Number of Physicians",
           fill = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    
    # Add percentage labels if checkbox is checked
    if (input$show_percent) {
      p <- p + geom_text(aes(label = paste0(percentage, "%")), 
                         vjust = -0.5, 
                         size = 3.5)
    }
    
    p
    
  })
  
}

    #  # filter data based on selected specialty
    # filtered_data <- specialtyByState %>%
    #   filter(specialty == input$n_breaks)
    # 
    # hist( state_facts$"specialty", #what makes the plot
    #       probability = TRUE, 
    #       breaks      = as.numeric(input$n_breaks), #important for making the graph dynamic/changeable on the website! this is where you are referring to the inputs
    #       xlab        = "States with Specialty Dominance",
    #       main        = "Specialty Prominance by State"
    # )
    # 
    # if (input$primary) { # if the thing inside this if statement is true, then this code will run
    #   rug(state_facts$"specialty")
    # }
    # 
    # if (input$primAndSec) {   # this responds to the check boxes on the webpage! when you check "Show density distribution," then this "if" statement runs
    #   dens <- density(state_facts$specialty)
    #   lines(dens, col = "red")
    # }
    


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




