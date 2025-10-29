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
  
     
##############################################
# 4 HEALTH RISK CALCULATOR:
     
# Reactive value to store calculation trigger
 risk_data <- eventReactive(input$calculate_risk, {
       
       # Get user inputs
       user_age <- input$input_age
       user_gender <- input$input_gender
       user_bp <- input$input_bp
       user_hr <- input$input_hr
       user_chol <- input$input_chol
       user_bmi <- input$input_bmi
       
       # Calculate percentiles
       age_percentile <- round(mean(healthcare_dataset$Age <= user_age) * 100, 1)
       bp_percentile <- round(mean(healthcare_dataset$Blood_Pressure <= user_bp) * 100, 1)
       hr_percentile <- round(mean(healthcare_dataset$Heart_Rate <= user_hr) * 100, 1)
       chol_percentile <- round(mean(healthcare_dataset$Cholesterol_Level <= user_chol) * 100, 1)
       bmi_percentile <- round(mean(healthcare_dataset$BMI <= user_bmi) * 100, 1)
       
       # Calculate risk score (higher is worse)
       risk_score <- 0
       if (user_bp > 140) risk_score <- risk_score + 2
       else if (user_bp > 120) risk_score <- risk_score + 1
       
       if (user_chol > 240) risk_score <- risk_score + 2
       else if (user_chol > 200) risk_score <- risk_score + 1
       
       if (user_bmi > 30) risk_score <- risk_score + 2
       else if (user_bmi > 25) risk_score <- risk_score + 1
       
       if (user_hr > 100) risk_score <- risk_score + 1
       else if (user_hr < 60) risk_score <- risk_score + 1
       
       # Determine risk level
       risk_level <- if (risk_score <= 2) "Low" else if (risk_score <= 4) "Medium" else "High"
       
       # Find similar patients (within ranges)
       similar_patients <- healthcare_dataset %>%
         filter(
           abs(Age - user_age) <= 10,
           Gender == user_gender,
           abs(BMI - user_bmi) <= 5
         )
       
       # Get diagnosis distribution for similar patients
       diagnosis_counts <- similar_patients %>%
         count(Diagnosis) %>%
         arrange(desc(n)) %>%
         mutate(percentage = round(n / sum(n) * 100, 1))
       
       # Calculate averages
       dataset_avg <- healthcare_dataset %>%
         summarise(
           avg_age = mean(Age, na.rm = TRUE),
           avg_bp = mean(Blood_Pressure, na.rm = TRUE),
           avg_hr = mean(Heart_Rate, na.rm = TRUE),
           avg_chol = mean(Cholesterol_Level, na.rm = TRUE),
           avg_bmi = mean(BMI, na.rm = TRUE)
         )
       
       list(
         percentiles = list(
           age = age_percentile,
           bp = bp_percentile,
           hr = hr_percentile,
           chol = chol_percentile,
           bmi = bmi_percentile
         ),
         risk_level = risk_level,
         risk_score = risk_score,
         similar_count = nrow(similar_patients),
         diagnosis_counts = diagnosis_counts,
         dataset_avg = dataset_avg,
         user_values = list(
           age = user_age,
           bp = user_bp,
           hr = user_hr,
           chol = user_chol,
           bmi = user_bmi
         )
       )
     })
     
     # Risk level box
     output$risk_level_box <- renderUI({
       data <- risk_data()
       
       risk_class <- switch(data$risk_level,
                            "Low" = "risk-low",
                            "Medium" = "risk-medium",
                            "High" = "risk-high")
       
       risk_message <- switch(data$risk_level,
                              "Low" = "Your health metrics look good! Keep maintaining healthy habits.",
                              "Medium" = "Some of your metrics are elevated. Consider consulting with a healthcare provider.",
                              "High" = "Multiple risk factors detected. We recommend consulting with a healthcare provider soon.")
       
       div(class = paste("risk-box", risk_class),
           h2(paste("Overall Risk Level:", data$risk_level)),
           p(risk_message)
       )
     })
     
     # Percentile rankings
     output$percentile_rankings <- renderUI({
       data <- risk_data()
       
       HTML(paste0(
         "<p><strong>Age:</strong> <span class='percentile-text'>", data$percentiles$age, "th</span> percentile</p>",
         "<p><strong>Blood Pressure:</strong> <span class='percentile-text'>", data$percentiles$bp, "th</span> percentile</p>",
         "<p><strong>Heart Rate:</strong> <span class='percentile-text'>", data$percentiles$hr, "th</span> percentile</p>",
         "<p><strong>Cholesterol:</strong> <span class='percentile-text'>", data$percentiles$chol, "th</span> percentile</p>",
         "<p><strong>BMI:</strong> <span class='percentile-text'>", data$percentiles$bmi, "th</span> percentile</p>"
       ))
     })
     
     # Similar patients info
     output$similar_patients_info <- renderUI({
       data <- risk_data()
       
       if (data$similar_count > 0) {
         HTML(paste0(
           "<p><strong>Similar Patients Found:</strong> ", data$similar_count, "</p>",
           "<p>Patients with similar age, gender, and BMI profiles in our dataset.</p>",
           "<p style='font-size: 14px; color: #666; margin-top: 10px;'>",
           "This helps us understand common diagnoses for people with your profile.</p>"
         ))
       } else {
         HTML("<p>No closely matching patient profiles found in dataset.</p>")
       }
     })
     
     # Diagnosis distribution chart
     output$diagnosis_distribution <- renderPlot({
       data <- risk_data()
       
       if (nrow(data$diagnosis_counts) > 0) {
         ggplot(data$diagnosis_counts, aes(x = reorder(Diagnosis, -n), y = n, fill = Diagnosis)) +
           geom_col() +
           geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, fontface = "bold") +
           labs(title = "Common Diagnoses for Similar Patient Profiles",
                x = "Diagnosis",
                y = "Number of Patients") +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                 legend.position = "none",
                 plot.title = element_text(size = 16, face = "bold"))
       } else {
         ggplot() +
           annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for similar profiles", size = 6) +
           theme_void()
       }
     })
     
     # Comparison chart
     output$comparison_chart <- renderPlot({
       data <- risk_data()
       
       comparison_data <- data.frame(
         Metric = c("Blood Pressure", "Heart Rate", "Cholesterol", "BMI"),
         Your_Value = c(data$user_values$bp, data$user_values$hr, 
                        data$user_values$chol, data$user_values$bmi),
         Dataset_Average = c(data$dataset_avg$avg_bp, data$dataset_avg$avg_hr,
                             data$dataset_avg$avg_chol, data$dataset_avg$avg_bmi)
       ) %>%
         pivot_longer(cols = c(Your_Value, Dataset_Average), 
                      names_to = "Type", 
                      values_to = "Value")
       
       ggplot(comparison_data, aes(x = Metric, y = Value, fill = Type)) +
         geom_col(position = "dodge", width = 0.7) +
         scale_fill_manual(values = c("Your_Value" = "#d32f2f", "Dataset_Average" = "#1976d2"),
                           labels = c("Dataset Average", "Your Value")) +
         labs(title = "Your Health Metrics vs. Dataset Averages",
              x = "",
              y = "Value",
              fill = "") +
         theme_minimal() +
         theme(axis.text.x = element_text(size = 12, face = "bold"),
               legend.position = "top",
               legend.text = element_text(size = 12),
               plot.title = element_text(size = 16, face = "bold"))
     })
     
     ##############################################     
} # THIS COVERS THE WHOLE CODE! (connected to function(input,output) around line 16)