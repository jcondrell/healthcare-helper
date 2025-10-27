# Libraries:
library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggpubr)
library(sf)
library(htmltools)


# Importing all csv datasets: 
healthcare_dataset <- read_csv("healthcare_dataset.csv") # you have to copy these lines and then run in the console to be able to see them in the environment!
specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE) # for the histogram (2)
specialtyByStateWithOther <- read.csv("specialtyByState_WithOther.csv", stringsAsFactors = FALSE) # for pie chart (3) (includes all specialties and "other" column in state_facts)


# BEGINNING OF MAIN CODE:
function(input, output) {
  

################################
# 1 CHLOROPLETH:
  output$main_plot <- renderLeaflet({
    states <- read_sf("us-states.geojson")
    
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      states$name, states$density
    ) %>% lapply(HTML)
    
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(density),
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
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
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