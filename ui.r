
# Libraries:
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)

# Importing all csv datasets: 
healthcare_dataset <- read_csv("healthcare_dataset.csv")
specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)
specialtyByStateWithOther <- read.csv("specialtyByState_WithOther.csv", stringsAsFactors = FALSE)

# BEGINNING OF MAIN SHINY CODES:
ui <- navbarPage(
  title = tags$span(style = "font-weight: bold; color: red;", tags$em("Healthcare Helper"), tags$sup("™")),
  
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
             /* Tab styling - Medical color palette */
             .navbar-default .navbar-nav > li > a {
               font-weight: bold;
               font-size: 15px;
             }
             /* Home tab - No special color (default) */
             .navbar-default .navbar-nav > li > a[data-value='Home'] {
               background-color: transparent !important;
               color: inherit !important;
             }
             /* Specialty Geographic Distribution - Deep Blue */
             .navbar-default .navbar-nav > li > a[data-value='Specialty Geographic Distribution'] {
               background-color: #1e88e5 !important;
               color: white !important;
             }
             /* Specialty State Rankings - Coral/Orange */
             .navbar-default .navbar-nav > li > a[data-value='Specialty State Rankings'] {
               background-color: #ff6f61 !important;
               color: white !important;
             }
             /* Specialty Distribution Analysis - Pink */
             .navbar-default .navbar-nav > li > a[data-value='Specialty Distribution Analysis'] {
               background-color: #ec407a !important;
               color: white !important;
             }
             /* Health Risk Calculator - Dark Red */
             .navbar-default .navbar-nav > li > a[data-value='Health Risk Calculator'] {
               background-color: #d32f2f !important;
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
             
             /* Hover effect for feature cards */
             .feature-card {
               transition: transform 0.3s ease, box-shadow 0.3s ease;
             }
             .feature-card:hover {
               transform: translateY(-5px);
             }
          "))
  ),
  
  # HOME TAB
  tabPanel("Home",
           tags$div(
             # Hero Section
             tags$div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 80px 20px; text-align: center; color: white; position: relative; overflow: hidden;",
                      # Animated background bubbles
                      tags$div(style = "position: absolute; top: 20%; left: 10%; width: 100px; height: 100px; background: rgba(255,255,255,0.1); border-radius: 50%; animation: float 6s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 60%; right: 15%; width: 150px; height: 150px; background: rgba(255,255,255,0.1); border-radius: 50%; animation: float 8s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 30%; right: 20%; width: 80px; height: 80px; background: rgba(255,255,255,0.08); border-radius: 50%; animation: float 7s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 70%; left: 25%; width: 120px; height: 120px; background: rgba(255,255,255,0.08); border-radius: 50%; animation: float 9s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 10%; left: 40%; width: 60px; height: 60px; background: rgba(255,255,255,0.12); border-radius: 50%; animation: float 5s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 50%; right: 8%; width: 90px; height: 90px; background: rgba(255,255,255,0.09); border-radius: 50%; animation: float 10s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 15%; right: 35%; width: 70px; height: 70px; background: rgba(255,255,255,0.11); border-radius: 50%; animation: float 6.5s ease-in-out infinite;"),
                      tags$div(style = "position: absolute; top: 80%; left: 5%; width: 110px; height: 110px; background: rgba(255,255,255,0.07); border-radius: 50%; animation: float 8.5s ease-in-out infinite;"),
                      
                      tags$div(style = "position: relative; z-index: 1;",
                               h1(tags$span("Welcome to ", tags$em("Healthcare Helper"), tags$sup("™")),
                                  style = "font-size: 58px; font-weight: bold; margin-bottom: 20px; text-shadow: 2px 2px 4px rgba(0,0,0,0.2);"),
                               p("Empowering you with data-driven insights for better healthcare decisions",
                                 style = "font-size: 22px; max-width: 700px; margin: 0 auto 30px; opacity: 0.95;"),
                               tags$div(style = "display: inline-block; background: rgba(255,255,255,0.2); padding: 15px 30px; border-radius: 50px; backdrop-filter: blur(10px);",
                                        tags$span(style = "font-size: 18px; font-weight: 500;", "🏥 Explore • 📊 Analyze • 💡 Decide"))
                      )
             ),
             
             # Features Grid
             tags$div(style = "max-width: 1200px; margin: 60px auto; padding: 0 20px;",
                      h2("What Can You Do?", style = "text-align: center; font-size: 42px; font-weight: bold; color: #2c3e50; margin-bottom: 50px;"),
                      
                      tags$div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 30px;",
                               # Feature Card 1 - Specialty Geographic Distribution (Deep Blue to match tab)
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(30,136,229,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "🗺️"),
                                        h3("Specialty Geographic Distribution", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Discover geographic distribution of medical specialties across the United States with interactive visualizations.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 2 - Specialty State Rankings (Coral/Orange to match tab)
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #ff6f61 0%, #e85d4f 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(255,111,97,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "📊"),
                                        h3("Specialty State Rankings", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Compare states to find where your desired medical specialty is most prominent and accessible.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 3 - Specialty Distribution Analysis (Pink to match tab)
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #ec407a 0%, #d81b60 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(236,64,122,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "🥧"),
                                        h3("Specialty Distribution Analysis", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Analyze specialty distribution within each state to understand local healthcare landscapes.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 4 - Health Risk Calculator (Dark Red to match tab)
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #d32f2f 0%, #b71c1c 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(211,47,47,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "❤️"),
                                        h3("Health Risk Calculator", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Get personalized health risk assessments based on your metrics and compare with similar patient profiles.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               )
                      )
             ),
             
             # Stats Section
             tags$div(style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); padding: 60px 20px; margin-top: 60px;",
                      tags$div(style = "max-width: 1000px; margin: 0 auto; display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 40px; text-align: center;",
                               tags$div(
                                 h3("50+", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
                                 p("States & Territories", style = "color: rgba(255,255,255,0.9); font-size: 18px;")
                               ),
                               tags$div(
                                 h3("10,000+", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
                                 p("Patient Records", style = "color: rgba(255,255,255,0.9); font-size: 18px;")
                               ),
                               tags$div(
                                 h3("15+", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
                                 p("Medical Specialties", style = "color: rgba(255,255,255,0.9); font-size: 18px;")
                               )
                      )
             ),
             
             # Medical Image Section
             tags$div(style = "text-align: center; padding: 60px 20px; background: white;",
                      tags$img(src = "https://images.unsplash.com/photo-1576091160399-112ba8d25d1d?w=800",
                               alt = "Medical stethoscope",
                               style = "width: 100%; max-width: 700px; border-radius: 15px; box-shadow: 0 10px 40px rgba(0,0,0,0.15);")
             ),
             
             # Call to Action
             tags$div(style = "text-align: center; padding: 80px 20px; background: #f8f9fa;",
                      h2("Ready to Get Started?", style = "font-size: 42px; font-weight: bold; color: #2c3e50; margin-bottom: 20px;"),
                      p("Click the tabs at the top to explore different features and insights!",
                        style = "font-size: 24px; color: #667eea; font-weight: 500; margin-bottom: 40px;"),
                      
                      # Fun animation arrow pointing up
                      tags$div(style = "margin-top: 30px;",
                               tags$div(style = "font-size: 60px; animation: bounce 2s infinite;", "☝️"),
                               tags$style(HTML("
                                 @keyframes bounce {
                                   0%, 20%, 50%, 80%, 100% { transform: translateY(0); }
                                   40% { transform: translateY(-20px); }
                                   60% { transform: translateY(-10px); }
                                 }
                               "))
                      )
             ),
             
             # CSS animations
             tags$style(HTML("
               @keyframes float {
                 0%, 100% { transform: translateY(0px); }
                 50% { transform: translateY(-20px); }
               }
             "))
           )
  ),
  
  
  
  
  ################################
  # 1 CHLOROPLETH:
  tabPanel("Specialty Geographic Distribution",
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
  tabPanel("Specialty State Rankings",
           
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
  tabPanel("Specialty Distribution Analysis",
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