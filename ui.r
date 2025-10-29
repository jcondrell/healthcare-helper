# Libraries:
library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)
library(leaflet)


# Importing all csv datasets: 
healthcare_dataset <- read_csv("healthcare_dataset.csv")
specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE) # for histogram
specialtyByStateWithOther <- read.csv("specialtyByState_WithOther.csv", stringsAsFactors = FALSE) # for pie chart (includes all specialties and "other" column in state_facts)


# BEGINNING OF MAIN SHINY CODES:
ui <- navbarPage(
  title = tags$span(style = "font-weight: bold; color: red;", tags$em("Healthcare Helper"), tags$sup("™")),
  
  # Really wanted to change all the fonts to Times New Roman... :
  tags$head(
    tags$style(HTML("
            body {
              font-family: 'Times New Roman', Times, serif;
             }
             .risk-box {
               padding: 20px;
               margin: 15px 0;
               border-radius: 10px;
               text-align: center;
               font-size: 18px;
               font-weight: bold;
             }
             .risk-low { background-color: #c8e6c9; color: #2e7d32; }
             .risk-medium { background-color: #fff9c4; color: #f57f17; }
             .risk-high { background-color: #ffcdd2; color: #c62828; }
             .metric-box {
               background-color: #f5f5f5;
               padding: 15px;
               margin: 10px 0;
               border-radius: 8px;
               border-left: 4px solid #d32f2f;
             }
             .percentile-text {
               font-size: 24px;
               font-weight: bold;
               color: #d32f2f;
             }
             .input-box {
               width: 100%;
               padding: 8px;
               font-size: 16px;
               border: 2px solid #ddd;
               border-radius: 5px;
               margin-bottom: 15px;
             }
             /* Tab styling */
             .navbar-default .navbar-nav > li > a {
               font-weight: bold;
               font-size: 15px;
             }
             .navbar-default .navbar-nav > li:nth-child(2) > a {
               background-color: #2C5F8D !important;
               color: white !important;
             }
             .navbar-default .navbar-nav > li:nth-child(3) > a {
               background-color: #E76F51 !important;
               color: white !important;
             }
             .navbar-default .navbar-nav > li:nth-child(4) > a {
               background-color: #90E0EF !important;
               color: white !important;
             }
             .navbar-default .navbar-nav > li:nth-child(5) > a {
               background-color: #F77F00 !important;
               color: white !important;
             }
             .navbar-default .navbar-nav > li:nth-child(6) > a {
               background-color: #A8DADC !important;
               color: white !important;
             }
             .navbar-default .navbar-nav > li.active > a {
               border-bottom: 4px solid #ffeb3b !important;
             }
             /* Show the Healthcare Helper brand */
             .navbar-brand {
               display: block !important;
               font-family: 'Times New Roman', Times, serif !important;
             }
          "))
  ),
  
  
  # Creating the tabs and the main header page:
  tabPanel("Home",
           tags$div(style = "text-align: center; padding: 60px 20px;",
                    h1(tags$span("Welcome to the ", tags$em("Healthcare Helper"), " App!"), #all this extra stuff is it italicizing it
                       style = "font-size: 56px; font-weight: bold; color: #d32f2f; margin-bottom: 40px;"),
                    p("We are here to help you find accessible healthcare data to better help you assess your healthcare needs and options!",
                      style = "font-size: 28px; line-height: 1.6; max-width: 900px; margin: 0 auto 50px;"),
                    
                    # Add image
                    tags$img(src = "https://images.unsplash.com/photo-1576091160399-112ba8d25d1d?w=800",
                             alt = "Medical stethoscope",
                             style = "width: 100%; max-width: 600px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);")
           )
  ),
  
  
  
  ################################
  # 1 CHLOROPLETH:
  tabPanel("Specialty Map by State",
           sidebarLayout(
             sidebarPanel(
               selectInput("specialty_select",
                           "Select Specialty:",
                           choices = unique(specialtyByState$specialty),
                           selected = unique(specialtyByState$specialty)[1])
             ),
             mainPanel(
               h3("Geographic Distribution of Selected Specialty"),
               p("This map shows what percentage of each state's physicians work in your selected specialty. 
          Colors range from purple (lower percentage) to yellow (higher percentage), making it easy 
          to identify where this specialty is most concentrated. Note: The 'Other Specialties' category is excluded from 
          the total physician count for this calculation."),
               leafletOutput("chloropleth_map", height = 600)
             )
           )
  ),
  ###################################
  
  
  ######################################
  # 2 THIS IS HISTOGRAM:
  tabPanel("Find most prominent specialties by state",
           
           sidebarLayout(
             sidebarPanel(  
               selectInput(inputId = "n_breaks",
                           label = "Pick desired specialty:",
                           choices = unique(specialtyByStateWithOther$specialty),
                           selected = c("Pick your desired specialty")
               ),
               checkboxInput(inputId = "show_percent",
                             label = strong("Show percentage of state's total physicians"),
                             value = FALSE
               )
             ),
             
             # this makes the heading within the tab:
             mainPanel(
               h3("Understanding Specialty Distribution"),
               p("This chart can help you find the top 10 states with the most physicians in your selected specialty. 
          States highlighted in red indicate where this specialty is the #1 most common specialty 
          in that state, while blue indicates that there is still a high quantity of physicians in that specialty, but it may not be #1 in that state."),
               plotOutput("main_plot")
             )
           )
           
  ),
  ##################################
  
  
  ######################################
  # 3 THIS IS THE PIE CHART PER STATE!
  tabPanel("State by state pie chart specialty analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("state_select",
                           "Select a State:",
                           choices = unique(specialtyByState$Location),
                           selected = unique(specialtyByState$Location)[1]),
               h4(textOutput("total_physicians")), # this will give the information on the total number of physcians in that selected state
             ),
             
             mainPanel(
               h3("Understanding specialty percentages within selected states:"),
               p("This pie chart allows you to select a state and see which specialties are most dominant in that state.
            Of note, 'Other Specialties' was included in this metrics to show hollistic distribution."),
               
               plotOutput("pie_chart")
             )
           )
  ),
  ##############################################
  
  
  ######################################
  # 4 HEALTH RISK CALCULATOR:
  tabPanel("Health Risk Calculator",
           fluidRow(
             column(12,
                    div(style = "text-align: center; padding: 30px 20px;",
                        h1("Personal Health Risk Assessment", 
                           style = "font-size: 42px; font-weight: bold; color: #d32f2f; margin-bottom: 20px;"),
                        p("Enter your health metrics below to see how you compare to our patient dataset and assess potential health risks.",
                          style = "font-size: 18px; color: #555; margin-bottom: 40px;")
                    )
             )
           ),
           
           sidebarLayout(
             sidebarPanel(
               h3("Enter Your Health Metrics:", style = "color: #d32f2f;"),
               
               numericInput("input_age",
                            "Age:",
                            value = 45,
                            min = 18,
                            max = 100,
                            step = 1),
               
               selectInput("input_gender",
                           "Gender:",
                           choices = c("Male", "Female"),
                           selected = "Male"),
               
               numericInput("input_bp",
                            "Blood Pressure (systolic):",
                            value = 120,
                            min = 80,
                            max = 200,
                            step = 1),
               
               numericInput("input_hr",
                            "Heart Rate (bpm):",
                            value = 70,
                            min = 40,
                            max = 150,
                            step = 1),
               
               numericInput("input_chol",
                            "Cholesterol Level (mg/dL):",
                            value = 200,
                            min = 100,
                            max = 400,
                            step = 1),
               
               numericInput("input_bmi",
                            "BMI:",
                            value = 25,
                            min = 15,
                            max = 50,
                            step = 0.1),
               
               actionButton("calculate_risk",
                            "Calculate My Risk Profile",
                            style = "background-color: #d32f2f; color: white; font-weight: bold; width: 100%; padding: 12px; font-size: 16px;")
             ),
             
             mainPanel(
               h3("Your Health Risk Profile:", style = "color: #d32f2f; margin-bottom: 20px;"),
               
               uiOutput("risk_level_box"),
               
               fluidRow(
                 column(6,
                        div(class = "metric-box",
                            h4(strong("Your Percentile Rankings:")),
                            htmlOutput("percentile_rankings")
                        )
                 ),
                 column(6,
                        div(class = "metric-box",
                            h4(strong("Similar Patient Profile:")),
                            htmlOutput("similar_patients_info")
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        h4(strong("If Seeking Medical Help: Common Diagnoses for Individuals with Similar Demographics"), style = "margin-top: 20px;"),
                        p("This chart shows the most frequent diagnoses among patients in our dataset with similar characteristics to yours (within ±10 years age, same gender, and ±5 BMI points). The percentages indicate what proportion of these similar patients received each diagnosis.",
                          style = "color: #666; font-size: 14px; margin-bottom: 10px; line-height: 1.5;"),
                        p(strong("Important Note:"), " This dataset only includes individuals who sought medical care at healthcare facilities. Many healthy individuals with similar profiles who did not require medical attention are not represented in this data. These percentages should not be interpreted as your likelihood of developing these conditions.",
                          style = "color: #d32f2f; font-size: 13px; margin-bottom: 15px; line-height: 1.5; background-color: #fff3e0; padding: 10px; border-radius: 5px; border-left: 4px solid #ff9800;"),
                        plotOutput("diagnosis_distribution", height = 300)
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(12,
                        h4(strong("How You Compare to Dataset Averages:"), style = "margin-top: 20px;"),
                        p("This comparison shows your health metrics (in red) side-by-side with the average values from our entire patient dataset (in blue). This helps you see where your values are higher or lower than typical patients in our database.",
                          style = "color: #666; font-size: 14px; margin-bottom: 15px; line-height: 1.5;"),
                        plotOutput("comparison_chart", height = 350)
                 )
               )
             )
           )
  )
  ##############################################
  
  
) # connects to navbarPage at top! must engulf WHOLE THING!