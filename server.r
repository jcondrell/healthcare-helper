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
       
       # Check for extreme values (outside dataset range)
       extreme_values <- list()
       
       if (user_age < min(healthcare_dataset$Age) || user_age > max(healthcare_dataset$Age)) {
         extreme_values$age <- paste0("Age (", user_age, ") is outside dataset range [", 
                                      min(healthcare_dataset$Age), "-", max(healthcare_dataset$Age), "]")
       }
       if (user_bp < min(healthcare_dataset$Blood_Pressure) || user_bp > max(healthcare_dataset$Blood_Pressure)) {
         extreme_values$bp <- paste0("Blood Pressure (", user_bp, ") is outside dataset range [", 
                                     min(healthcare_dataset$Blood_Pressure), "-", max(healthcare_dataset$Blood_Pressure), "]")
       }
       # Only flag heart rate as extreme if above 130 (not just above dataset max)
       if (user_hr < min(healthcare_dataset$Heart_Rate) || user_hr >= 130) {
         extreme_values$hr <- paste0("Heart Rate (", user_hr, ") is outside safe range")
       }
       if (user_chol < min(healthcare_dataset$Cholesterol_Level) || user_chol > max(healthcare_dataset$Cholesterol_Level)) {
         extreme_values$chol <- paste0("Cholesterol (", user_chol, ") is outside dataset range [", 
                                       min(healthcare_dataset$Cholesterol_Level), "-", max(healthcare_dataset$Cholesterol_Level), "]")
       }
       if (user_bmi < min(healthcare_dataset$BMI) || user_bmi > max(healthcare_dataset$BMI)) {
         extreme_values$bmi <- paste0("BMI (", user_bmi, ") is outside dataset range [", 
                                      min(healthcare_dataset$BMI), "-", max(healthcare_dataset$BMI), "]")
       }
       
       # Calculate percentiles
       age_percentile <- round(mean(healthcare_dataset$Age <= user_age) * 100, 1)
       bp_percentile <- round(mean(healthcare_dataset$Blood_Pressure <= user_bp) * 100, 1)
       hr_percentile <- round(mean(healthcare_dataset$Heart_Rate <= user_hr) * 100, 1)
       chol_percentile <- round(mean(healthcare_dataset$Cholesterol_Level <= user_chol) * 100, 1)
       bmi_percentile <- round(mean(healthcare_dataset$BMI <= user_bmi) * 100, 1)
       
       # Calculate risk score (higher is worse)
       risk_score <- 0
       
       # Blood Pressure scoring
       if (user_bp > 140) risk_score <- risk_score + 2
       else if (user_bp > 120) risk_score <- risk_score + 1
       
       # Cholesterol scoring
       if (user_chol > 240) risk_score <- risk_score + 2
       else if (user_chol > 200) risk_score <- risk_score + 1
       
       # BMI scoring
       if (user_bmi > 30) risk_score <- risk_score + 2
       else if (user_bmi > 25) risk_score <- risk_score + 1
       
       # Heart Rate scoring (hardcoded thresholds)
       if (user_hr > 120) risk_score <- risk_score + 2
       else if (user_hr >= 100) risk_score <- risk_score + 1
       else if (user_hr < 60) risk_score <- risk_score + 1
       
       # Age scoring (cardiovascular risk increases with age)
       if (user_age >= 65) risk_score <- risk_score + 2
       else if (user_age >= 50) risk_score <- risk_score + 1
       
       # Determine risk level (override if extreme values detected)
       risk_level <- if (length(extreme_values) > 0) {
         "Potential High Risk"
       } else if (risk_score <= 2) {
         "Low"
       } else if (risk_score == 4) {
         "Medium"
       } else {
         "High"
       }
       
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
         ),
         extreme_values = extreme_values
       )
     })
     
     # Risk level box
     output$risk_level_box <- renderUI({
       data <- risk_data()
       
       risk_class <- switch(data$risk_level,
                            "Low" = "risk-low",
                            "Medium" = "risk-medium",
                            "High" = "risk-high",
                            "Potential High Risk" = "risk-high")
       
       risk_message <- switch(data$risk_level,
                              "Low" = "Your health metrics look good! Keep maintaining healthy habits.",
                              "Medium" = "Some of your metrics are elevated. Consider consulting with a healthcare provider.",
                              "High" = "Multiple risk factors detected. We recommend consulting with a healthcare provider soon.",
                              "Potential High Risk" = "One or more of your values is outside our dataset's range. Please consult with a healthcare provider immediately.")
       
       # Create extreme values warning if any exist
       extreme_warning <- if (length(data$extreme_values) > 0) {
         extreme_list <- paste(unlist(data$extreme_values), collapse = "<br/>")
         HTML(paste0(
           "<div style='background-color: #ffebee; padding: 15px; margin-top: 15px; border-radius: 5px; border: 2px solid #c62828;'>",
           "<h4 style='color: #c62828; margin-top: 0;'>⚠️ Extreme Values Detected:</h4>",
           "<p style='color: #c62828; margin: 0;'><strong>", extreme_list, "</strong></p>",
           "<p style='margin-top: 10px; margin-bottom: 0; font-size: 14px;'>Your values exceed the range of data in our dataset. This may indicate a serious health concern. Please seek immediate medical attention.</p>",
           "</div>"
         ))
       } else {
         ""
       }
       
       tagList(
         div(class = paste("risk-box", risk_class),
             h2(strong(paste("Overall Risk Level:", data$risk_level))),
             p(risk_message)
         ),
         HTML(extreme_warning)
       )
     })
     
     # Percentile rankings
     output$percentile_rankings <- renderUI({
       data <- risk_data()
       
       # Function to color code percentiles if extreme
       make_percentile_text <- function(name, percentile, is_extreme) {
         color <- if (is_extreme) "#c62828" else "#d32f2f"
         background <- if (is_extreme) "#ffebee" else "transparent"
         style_extra <- if (is_extreme) "font-weight: bold; padding: 5px; border-radius: 3px;" else ""
         
         paste0("<p style='background-color: ", background, "; ", style_extra, "'>",
                "<strong style='font-weight: bold;'>", name, ":</strong> ",
                "<span class='percentile-text' style='color: ", color, "; font-weight: bold;'>", 
                percentile, "th</span> percentile",
                if (is_extreme) " <span style='color: #c62828; font-weight: bold;'>⚠️ EXTREME</span>" else "",
                "</p>")
       }
       
       HTML(paste0(
         make_percentile_text("Age", data$percentiles$age, !is.null(data$extreme_values$age)),
         make_percentile_text("Blood Pressure", data$percentiles$bp, !is.null(data$extreme_values$bp)),
         make_percentile_text("Heart Rate", data$percentiles$hr, !is.null(data$extreme_values$hr)),
         make_percentile_text("Cholesterol", data$percentiles$chol, !is.null(data$extreme_values$chol)),
         make_percentile_text("BMI", data$percentiles$bmi, !is.null(data$extreme_values$bmi))
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
         # Define color mapping for diagnoses (red = bad, green = good)
         diagnosis_colors <- c(
           "Healthy" = "#2ECC71",                    # Green
           "Hypertension" = "#F39C12",               # Orange
           "Hyperlipidemia" = "#E67E22",             # Dark Orange
           "Diabetes" = "#E74C3C",                   # Red-Orange
           "Coronary Artery Disease" = "#C0392B"     # Dark Red
         )
         
         ggplot(data$diagnosis_counts, aes(x = reorder(Diagnosis, -n), y = n, fill = Diagnosis)) +
           geom_col() +
           geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, fontface = "bold") +
           labs(x = "Diagnosis",
                y = "Number of Patients") +
           scale_fill_manual(values = diagnosis_colors) +  # Add this line
           scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                 legend.position = "none",
                 plot.margin = margin(t = 20, r = 20, b = 20, l = 20))
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
         geom_text(aes(label = round(Value, 1)), 
                   position = position_dodge(width = 0.7), 
                   vjust = -0.5, 
                   fontface = "bold",
                   size = 4) +
         scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
         scale_fill_manual(values = c("Your_Value" = "#d32f2f", "Dataset_Average" = "#1976d2"),
                           labels = c("Dataset Average", "Your Value")) +
         labs(x = "",
              y = "Value",
              fill = "") +
         theme_minimal() +
         theme(axis.text.x = element_text(size = 12, face = "bold"),
               legend.position = "top",
               legend.text = element_text(size = 12),
               plot.margin = margin(t = 20, r = 20, b = 20, l = 20))
     })
     
     ##############################################
     
     
     ######################################
     # 4 TREATMENT PATHWAYS DASHBOARD:
     
     # Treatment pie chart
     output$treatment_pie <- renderPlot({
       
       # Check what the actual column name is
       treatment_col <- names(healthcare_dataset)[grep("treatment", names(healthcare_dataset), ignore.case = TRUE)]
       
       # Filter data based on selected diagnosis
       plot_data <- if (input$treatment_diagnosis == "All Diagnoses") {
         healthcare_dataset
       } else {
         healthcare_dataset %>% filter(Diagnosis == input$treatment_diagnosis)
       }
       
       # Filter by selected treatment types
       plot_data <- plot_data %>%
         filter(.data[[treatment_col]] %in% input$treatment_types)
       
       if (nrow(plot_data) == 0) {
         ggplot() +
           annotate("text", x = 0.5, y = 0.5, 
                    label = "No data matches your filters 🤷", 
                    size = 5, color = "#6b7280") +
           theme_void()
       } else {
         # Calculate percentages
         treatment_summary <- plot_data %>%
           count(.data[[treatment_col]]) %>%
           rename(Treatment = 1) %>%
           mutate(percentage = n / sum(n) * 100,
                  label = paste0(round(percentage, 1), "%"))
         
         # Define treatment colors
         treatment_colors <- c(
           "Surgery" = "#ef4444",
           "Medication" = "#3b82f6",
           "Lifestyle Changes" = "#10b981",
           "Observation" = "#f59e0b",
           "None" = "#9ca3af"
         )
         
         ggplot(treatment_summary, aes(x = "", y = n, fill = Treatment)) +
           geom_bar(stat = "identity", width = 1) +
           coord_polar("y", start = 0) +
           geom_text(aes(label = label), 
                     position = position_stack(vjust = 0.5),
                     color = "white",
                     fontface = "bold",
                     size = 5) +
           scale_fill_manual(values = treatment_colors) +
           labs(fill = "") +
           theme_void() +
           theme(legend.position = "bottom",
                 legend.text = element_text(size = 12),
                 plot.margin = margin(10, 10, 10, 10))
       }
     })
     
     # Treatment statistics
     output$treatment_stats <- renderUI({
       
       treatment_col <- names(healthcare_dataset)[grep("treatment", names(healthcare_dataset), ignore.case = TRUE)]
       
       plot_data <- if (input$treatment_diagnosis == "All Diagnoses") {
         healthcare_dataset
       } else {
         healthcare_dataset %>% filter(Diagnosis == input$treatment_diagnosis)
       }
       
       plot_data <- plot_data %>%
         filter(.data[[treatment_col]] %in% input$treatment_types)
       
       if (nrow(plot_data) == 0) {
         HTML("<p style='color: #6b7280; text-align: center; padding: 40px;'>
          <span style='font-size: 48px;'>📭</span><br>
          No data available<br>
          <small>Try changing your filters</small></p>")
       } else {
         total_patients <- nrow(plot_data)
         most_common <- plot_data %>%
           count(.data[[treatment_col]]) %>%
           arrange(desc(n)) %>%
           slice(1) %>%
           rename(Treatment = 1)
         
         # Color for most common treatment
         treatment_emoji <- case_when(
           most_common$Treatment == "Surgery" ~ "🔴",
           most_common$Treatment == "Medication" ~ "🔵",
           most_common$Treatment == "Lifestyle Changes" ~ "🟢",
           most_common$Treatment == "Observation" ~ "🟠",
           most_common$Treatment == "None" ~ "⚪",
           TRUE ~ "💊"
         )
         
         HTML(paste0(
           "<div style='padding: 20px; text-align: center;'>",
           "<div style='font-size: 56px; margin-bottom: 10px;'>", treatment_emoji, "</div>",
           "<div style='font-size: 32px; font-weight: bold; color: #10b981; margin-bottom: 15px;'>", 
           format(total_patients, big.mark = ","), "</div>",
           "<div style='color: #6b7280; font-size: 14px; margin-bottom: 20px;'>Total Patients</div>",
           "<div style='background: #f0fdf4; padding: 15px; border-radius: 8px; text-align: left;'>",
           "<div style='color: #374151; font-weight: 500; margin-bottom: 5px;'>Most Common:</div>",
           "<div style='color: #10b981; font-size: 18px; font-weight: bold;'>", 
           most_common$Treatment, "</div>",
           "<div style='color: #6b7280; font-size: 14px;'>", 
           round(most_common$n / total_patients * 100, 1), "% of cases</div>",
           "</div>",
           "</div>"
         ))
       }
     })
     
     # Treatment demographics comparison
     output$treatment_demographics <- renderPlot({
       
       treatment_col <- names(healthcare_dataset)[grep("treatment", names(healthcare_dataset), ignore.case = TRUE)]
       
       plot_data <- if (input$treatment_diagnosis == "All Diagnoses") {
         healthcare_dataset
       } else {
         healthcare_dataset %>% filter(Diagnosis == input$treatment_diagnosis)
       }
       
       plot_data <- plot_data %>%
         filter(.data[[treatment_col]] %in% input$treatment_types)
       
       if (nrow(plot_data) == 0) {
         ggplot() +
           annotate("text", x = 0.5, y = 0.5, 
                    label = "No data matches your filters 🤷", 
                    size = 5, color = "#6b7280") +
           theme_void()
       } else {
         # Calculate average metrics by treatment
         avg_metrics <- plot_data %>%
           group_by(.data[[treatment_col]]) %>%
           summarise(
             Age = mean(Age, na.rm = TRUE),
             `Blood Pressure` = mean(Blood_Pressure, na.rm = TRUE),
             `Heart Rate` = mean(Heart_Rate, na.rm = TRUE),
             BMI = mean(BMI, na.rm = TRUE)
           ) %>%
           rename(Treatment = 1) %>%
           pivot_longer(cols = -Treatment, 
                        names_to = "Metric", 
                        values_to = "Value")
         
         ggplot(avg_metrics, aes(x = Metric, y = Value, fill = Treatment)) +
           geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
           scale_fill_manual(values = c(
             "Surgery" = "#ef4444",
             "Medication" = "#3b82f6",
             "Lifestyle Changes" = "#10b981",
             "Observation" = "#f59e0b",
             "None" = "#9ca3af"
           )) +
           labs(x = "", y = "Average Value", fill = "") +
           theme_minimal() +
           theme(axis.text.x = element_text(size = 11),
                 axis.text.y = element_text(size = 10),
                 legend.position = "bottom",
                 legend.text = element_text(size = 11),
                 panel.grid.minor = element_blank(),
                 plot.margin = margin(10, 10, 10, 10))
       }
     })
     
    
     
     
     
     ##############################################
     
} # THIS COVERS THE WHOLE CODE!