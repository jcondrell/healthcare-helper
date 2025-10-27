# Libraries:
library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet) # for chloropleth
library(dplyr)
library(ggpubr)
library(sf) # for chloropleth
library(htmltools) # for chloropleth


# Importing all csv datasets: 
healthcare_dataset <- read_csv("healthcare_dataset.csv") # you have to copy these lines and then run in the console to be able to see them in the environment!
specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE) # for the histogram (2)
specialtyByStateWithOther <- read.csv("specialtyByState_WithOther.csv", stringsAsFactors = FALSE) # for pie chart (3) (includes all specialties and "other" column in state_facts)


# BEGINNING OF MAIN CODE:
function(input, output) {
  

################################
# 1 CHLOROPLETH:
  
  output$chloropleth_map <- renderLeaflet({
    
    # Read the states geographic data
    states <- read_sf("us-states.geojson")
    
    # Calculate total physicians per state
    state_totals <- specialtyByState %>%
      filter(Location != "United States") %>%
      group_by(Location) %>%
      summarise(total_physicians = sum(physicianNumbers))
    
    # Get data for selected specialty and calculate percentage
    specialty_data <- specialtyByState %>%
      filter(specialty == input$specialty_select, Location != "United States") %>%
      left_join(state_totals, by = "Location") %>%
      mutate(percentage = (physicianNumbers / total_physicians) * 100) %>%
      select(Location, physicianNumbers, percentage)
    
    # Join with geographic data
    states_with_data <- states %>%
      left_join(specialty_data, by = c("name" = "Location"))
    
    # Create color palette based on PERCENTAGE instead of raw numbers
    bins <- c(0, 2, 4, 6, 8, 10, 12, 15, 17, 20, 25, 30, Inf)
    pal <- colorBin("viridis", domain = states_with_data$percentage, bins = bins)
    
    # Create labels showing both percentage and raw numbers
    labels <- sprintf(
      "<strong>%s</strong><br/>%.1f%% of physicians (%s total)<br/>in %s",
      states_with_data$name, 
      states_with_data$percentage,
      format(states_with_data$physicianNumbers, big.mark = ","),
      input$specialty_select
    ) %>% lapply(HTML)
    
    # Create map
    leaflet(states_with_data) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(percentage),  # Changed to percentage
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(
        pal = pal, 
        values = ~percentage,  # Changed to percentage
        opacity = 0.7, 
        title = paste(input$specialty_select, "(% of state)"),
        position = "bottomright")
  })
  
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14))
    
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
    
    state_data <- specialtyByStateWithOther %>%
      filter(Location == input$state_select) %>%
      mutate(
        percentage = physicianNumbers / sum(physicianNumbers) * 100,
        label = paste0(round(percentage, 1), "%")
      )
    
    ggplot(state_data, aes(x = "", y = physicianNumbers, fill = specialty)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(x = 1.7, label = label), # added in x = 1.7 so that the percentages were a bit farther away
                position = position_stack(vjust = 0.5), 
                color = "black", # since the numbers are now outside the chart, changed this to black
                fontface = "bold",
                size = 5) +
      labs(title = paste(input$state_select, "Distribution of Physicians"),
           fill = "Specialty") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold", margin = margin(b = 20)),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12),
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20))  # Add margins around entire plot
  })
  
  
# This is a bit to give the bit of information on the total number of physicians in the state:
     output$total_physicians <- renderText({
      total <- specialtyByState %>%
        filter(Location == input$state_select) %>%
        summarise(total = sum(physicianNumbers)) %>%
        pull(total)
      
      paste0("Total Physicians in ", input$state_select, ": ", format(total, big.mark = ",")) #used paste0 here instead of just paste to make the weird space in between the state listed and the colon
     })

##############################################
  
} # THIS COVERS THE WHOLE CODE! (connected to function(input,output) around line 16)