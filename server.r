library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggpubr)



healthcare_dataset <- read_csv("healthcare_dataset.csv") # you have to copy these lines and then run in the console to be able to see them in the environment!
state_facts <- read_csv("state_facts_handcleaned.csv")

specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)


function(input, output) {
  

################################
# 1 CHLOROPLETH:
# geo <- geojson_read("states.geo.json", what = "sp") 
# we will wait on this cloropleth for MONDAY!!!! - "See which specialties are prominent in each state!"
#######################################


######################################
# 2 THIS IS HISTOGRAM:

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
  
##################################



######################################
# 3 THIS IS THE PIE CHART PER STATE!

  output$pie_chart <- renderPlot({
    
    state_data <- specialtyByState %>%
      filter(state == input$state_select) %>%
      mutate(
        percentage = physicianNumbers / sum(physicianNumbers) * 100,
        label = paste0(round(percentage, 1), "%")
      )
    
    ggplot(state_data, aes(x = "", y = physicianNumbers, fill = specialty)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5),
                color = "white",
                fontface = "bold",
                size = 4) +
      labs(title = paste("Distribution of Medical Specialties in", input$state_select),
           fill = "Specialty") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    
  })


##############################################
  
} # THIS COVERS THE WHOLE CODE! (connected to function(input,output) around line 16)