# Libraries:
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)

# Importing all csv datasets: 
healthcare_dataset <- read_csv("healthcare_dataset.csv")
specialtyByState <- read.csv("specialtyByState.csv", stringsAsFactors = FALSE)
specialtyByStateWithOther <- read.csv("specialtyByState_WithOther.csv", stringsAsFactors = FALSE)

# Clean up specialty names for better display
specialtyByState$specialty <- gsub("\\.", " ", specialtyByState$specialty)
specialtyByState$specialty <- gsub("Oncology  Cancer ", "Oncology (Cancer)", specialtyByState$specialty)
specialtyByState$specialty <- gsub("Endocrinology  Diabetes  and Metabolism", "Endocrinology, Diabetes, and Metabolism", specialtyByState$specialty)

specialtyByStateWithOther$specialty <- gsub("\\.", " ", specialtyByStateWithOther$specialty)
specialtyByStateWithOther$specialty <- gsub("Oncology  Cancer ", "Oncology (Cancer)", specialtyByStateWithOther$specialty)
specialtyByStateWithOther$specialty <- gsub("Endocrinology  Diabetes  and Metabolism", "Endocrinology, Diabetes, and Metabolism", specialtyByStateWithOther$specialty)

# BEGINNING OF MAIN SHINY CODES:
ui <- navbarPage(
  title = tags$span(style = "font-weight: bold; color: red;", tags$em("Healthcare Helper"), tags$sup("™")),
  
  header = tagList(
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
             .treatment-box {
               background-color: #f5f5f5;
               padding: 15px;
               margin: 10px 0;
               border-radius: 8px;
               border-left: 4px solid #10b981;
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
             /* Health Metrics by Diagnosis - Purple */
             .navbar-default .navbar-nav > li > a[data-value='Health Metrics by Diagnosis'] {
               background-color: #8b5cf6 !important;
               color: white !important;
             }
             /* Treatment Pathways - Emerald Green */
             .navbar-default .navbar-nav > li > a[data-value='Treatment Pathways'] {
               background-color: #10b981 !important;
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
               cursor: pointer;
             }
             .feature-card:hover {
               transform: translateY(-5px);
             }
          "))

  ),
  tags$script(HTML("
      $(document).ready(function() {
      $('.feature-card').click(function() {
        var cardTitle = $(this).find('h3').text();
        var tabName;
        
        if (cardTitle.includes('Specialty Geographic Distribution')) {
          tabName = 'Specialty Geographic Distribution';
        } else if (cardTitle.includes('Specialty State Rankings')) {
          tabName = 'Specialty State Rankings';
        } else if (cardTitle.includes('Specialty Distribution Analysis')) {
          tabName = 'Specialty Distribution Analysis';
        } else if (cardTitle.includes('Treatment Pathways')) {
          tabName = 'Treatment Pathways';
        } else if (cardTitle.includes('Health Risk Calculator')) {
          tabName = 'Health Risk Calculator';
        }
        
        $('a[data-value=\"' + tabName + '\"]').tab('show');
      });
    });
  ")),
  ),
  
  
  # HOME TAB
  tabPanel("Home",
           tags$div(
             # Hero Section
             tags$div(style = "background: linear-gradient(135deg, #1e3a8a 0%, #1e40af 100%); padding: 80px 20px; text-align: center; color: white; position: relative; overflow: hidden;",
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
                                        tags$span(style = "font-size: 18px; font-weight: 500;", "Explore • Analyze • Decide"))
                      )
             ),
             
             # Features Grid
             tags$div(style = "max-width: 1200px; margin: 60px auto; padding: 0 20px;",
                      h2("What can you do?", style = "text-align: center; font-size: 42px; font-weight: bold; color: #2c3e50; margin-bottom: 50px;"),
                      
                      tags$div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 30px;",
                               # Feature Card 1 - Specialty Geographic Distribution
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(30,136,229,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "🗺️"),
                                        h3("Specialty Geographic Distribution", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Discover geographic distribution of medical specialties across the United States with interactive visualizations.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 2 - Specialty State Rankings
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #ff6f61 0%, #e85d4f 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(255,111,97,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "📊"),
                                        h3("Specialty State Rankings", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Compare states to find where your desired medical specialty is most prominent and accessible.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 3 - Specialty Distribution Analysis
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #ec407a 0%, #d81b60 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(236,64,122,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "🎯"),
                                        h3("Specialty Distribution Analysis", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Analyze specialty distribution within each state to understand local healthcare landscapes.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 5 - Health Metrics by Diagnosis
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #8b5cf6 0%, #6d28d9 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(139,92,246,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "📈"),
                                        h3("Health Metrics by Diagnosis", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Compare health indicators across different medical conditions and understand typical metric ranges.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 4 - Treatment Pathways
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #10b981 0%, #059669 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(16,185,129,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "💊"),
                                        h3("Treatment Pathways", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Explore treatment options and patterns for different diagnoses with comprehensive data analysis.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               ),
                               
                               # Feature Card 5 - Health Risk Calculator
                               tags$div(class = "feature-card",
                                        style = "background: linear-gradient(135deg, #d32f2f 0%, #b71c1c 100%); padding: 30px; border-radius: 15px; box-shadow: 0 10px 30px rgba(211,47,47,0.3); color: white;",
                                        tags$div(style = "font-size: 48px; margin-bottom: 15px;", "🤍"),
                                        h3("Health Risk Calculator", style = "color: white; font-size: 24px; margin-bottom: 15px; font-weight: bold;"),
                                        p("Get personalized health risk assessments based on your metrics and compare with similar patient profiles.",
                                          style = "color: rgba(255,255,255,0.9); line-height: 1.6; font-size: 16px;")
                               )
                      )
             ),
             
             # Stats Section
             tags$div(style = "background: linear-gradient(135deg, #1e3a8a 0%, #1e40af 100%); padding: 60px 20px; margin-top: 60px;",
                      tags$div(style = "max-width: 1000px; margin: 0 auto; display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 40px; text-align: center;",
                               tags$div(
                                 h3("51", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
                                 p("States & Territories", style = "color: rgba(255,255,255,0.9); font-size: 18px;")
                               ),
                               tags$div(
                                 h3("500", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
                                 p("Patient Records", style = "color: rgba(255,255,255,0.9); font-size: 18px;")
                               ),
                               tags$div(
                                 h3("8", style = "font-size: 48px; font-weight: bold; color: white; margin-bottom: 10px;"),
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
             
             # Next steps
             tags$div(style = "text-align: center; padding: 80px 20px; background: #f8f9fa;",
                      h2("Ready to get started?", style = "font-size: 42px; font-weight: bold; color: #2c3e50; margin-bottom: 20px;"),
                      p("Click the tabs at the top to explore different features and insights!",
                        style = "font-size: 24px; color: #667eea; font-weight: 500; margin-bottom: 40px;"),
                      
                      # Animation arrow pointing up
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
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #1e88e5 0%, #1565c0 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Specialty Geographic Distribution", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Explore where medical specialties are concentrated across the United States",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  fluidRow(
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Select Specialty:", style = "color: #1e88e5; margin-bottom: 20px;"),
                                    selectInput("specialty_select",
                                                NULL,
                                                choices = unique(specialtyByState$specialty),
                                                selected = unique(specialtyByState$specialty)[1]),
                                    
                                    hr(style = "border-color: #e5e7eb;"),
                                    
                                    tags$div(style = "background: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #1e88e5;",
                                             h5("About This Map:", style = "color: #1565c0; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "Colors show the percentage of each state's physicians in the selected specialty.", br(), br(),
                                               "🟣 Purple = Lower %", br(),
                                               "🟡 Yellow = Higher %", br(), br(),
                                               "Note: 'Other Specialties' excluded from calculations.")
                                    )
                           )
                    ),
                    
                    column(9,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Interactive Geographic Map", style = "color: #374151; margin-bottom: 15px;"),
                                    leafletOutput("chloropleth_map", height = 600)
                           )
                    )
                  )
         )
),
###################################
  
  
######################################
# 2 THIS IS HISTOGRAM:
tabPanel("Specialty State Rankings",
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #ff6f61 0%, #e85d4f 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Specialty State Rankings", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Compare states to find where medical specialties are most prominent",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  fluidRow(
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); height: 100%;",
                                    h4("Filter Rankings:", style = "color: #ff6f61; margin-bottom: 20px;"),
                                    
                                    selectInput(inputId = "n_breaks",
                                                label = "Select Specialty:",
                                                choices = unique(specialtyByStateWithOther$specialty),
                                                selected = c("Pick your desired specialty")
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb;"),
                                    
                                    h5("Display Options:", style = "color: #ff6f61; margin-top: 20px; margin-bottom: 15px;"),
                                    
                                    checkboxInput(inputId = "show_percent",
                                                  label = strong("Show percentage of state's total physicians"),
                                                  value = FALSE
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb; margin-top: 20px;"),
                                    
                                    tags$div(style = "background: #fff5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #ff6f61;",
                                             h5("Color Guide:", style = "color: #e85d4f; margin-top: 0;"),
                                             p(style = "font-size: 13px; color: #374151; margin: 5px 0; line-height: 1.8;",
                                               tags$span(style = "display: inline-block; width: 20px; height: 20px; background: #ff6b6b; border-radius: 3px; vertical-align: middle; margin-right: 8px;"), 
                                               tags$strong("Red:"), " #1 specialty in that state", br(),
                                               tags$span(style = "display: inline-block; width: 20px; height: 20px; background: #4dabf7; border-radius: 3px; vertical-align: middle; margin-right: 8px; margin-top: 8px;"), 
                                               tags$strong("Blue:"), " High count but not #1")
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb; margin-top: 20px;"),
                                    
                                    tags$div(style = "background: #fef3c7; padding: 15px; border-radius: 8px; border-left: 4px solid #f59e0b;",
                                             h5("About Rankings:", style = "color: #d97706; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "Shows the top 10 states with the highest number of physicians in your selected specialty.", br(), br(),
                                               "Enable percentages to see what proportion of each state's total physicians work in this field.")
                                    )
                           )
                    ),
                    
                    column(9,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Top 10 States by Physician Count", style = "color: #374151; margin-bottom: 20px;"),
                                    plotOutput("main_plot", height = 500)
                           )
                    )
                  )
         )
),
##################################


######################################
# 3 THIS IS THE PIE CHART PER STATE!
tabPanel("Specialty Distribution Analysis",
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #ec407a 0%, #d81b60 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Specialty Distribution Analysis", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Analyze how medical specialties are distributed within each state",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  fluidRow(
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); height: 100%;",
                                    h4("Select Location:", style = "color: #ec407a; margin-bottom: 20px;"),
                                    
                                    selectInput("state_select",
                                                "Choose a State:",
                                                choices = unique(specialtyByState$Location),
                                                selected = unique(specialtyByState$Location)[1]),
                                    
                                    hr(style = "border-color: #e5e7eb; margin: 25px 0;"),
                                    
                                    tags$div(style = "background: linear-gradient(135deg, #fce4ec 0%, #f8bbd0 100%); padding: 20px; border-radius: 8px; border-left: 4px solid #ec407a; text-align: center;",
                                             h5("State Summary", style = "color: #d81b60; margin-top: 0; margin-bottom: 15px;"),
                                             tags$div(style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 10px;",
                                                      h3(textOutput("total_physicians"), style = "color: #ec407a; margin: 0; font-size: 24px; font-weight: bold;")
                                             ),
                                             p(style = "font-size: 11px; color: #880e4f; margin: 5px 0;", "Total physicians in selected state")
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb; margin: 25px 0;"),
                                    
                                    tags$div(style = "background: #fce4ec; padding: 15px; border-radius: 8px; border-left: 4px solid #ec407a;",
                                             h5("Understanding the Data:", style = "color: #d81b60; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "• Each slice shows a medical specialty", br(),
                                               "• Percentages show portion of state's physicians", br(),
                                               "• 'Other Specialties' groups smaller categories", br(), br(),
                                               tags$strong("Use this to:"), br(),
                                               "✓ See dominant specialties", br(),
                                               "✓ Compare specialty availability", br(),
                                               "✓ Understand healthcare landscape")
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb; margin: 25px 0;"),
                                    
                                    tags$div(style = "background: #e1f5fe; padding: 15px; border-radius: 8px; border-left: 4px solid #0288d1;",
                                             h5("Chart Features:", style = "color: #01579b; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "• Color-coded slices for easy viewing", br(),
                                               "• Percentages displayed on chart", br(),
                                               "• Legend shows all specialties")
                                    )
                           )
                    ),
                    
                    column(9,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Physician Distribution by Specialty", style = "color: #374151; margin-bottom: 20px;"),
                                    plotOutput("pie_chart", height = 550)
                           )
                    )
                  )
         )
),
##############################################


######################################
# 6 HEALTH METRICS BY DIAGNOSIS TAB
tabPanel("Health Metrics by Diagnosis",
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #8b5cf6 0%, #6d28d9 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Health Metrics by Diagnosis", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Compare how health indicators differ across medical conditions",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         # Main content
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  
                  # Filter section
                  fluidRow(
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Select Metric:", style = "color: #8b5cf6; margin-bottom: 20px;"),
                                    
                                    selectInput("metric_select",
                                                "Health Metric to Analyze:",
                                                choices = c("Age", "Blood Pressure", "Heart Rate", 
                                                            "Cholesterol", "BMI"),
                                                selected = "BMI"),
                                    
                                    hr(style = "border-color: #e5e7eb;"),
                                    
                                    h5("Filter Diagnoses:", style = "color: #8b5cf6; margin-bottom: 15px;"),
                                    
                                    checkboxGroupInput("diagnosis_filter",
                                                       "Show:",
                                                       choices = c("Healthy", "Hypertension", "Hyperlipidemia", 
                                                                   "Diabetes", "Coronary Artery Disease"),
                                                       selected = c("Healthy", "Hypertension", "Hyperlipidemia", 
                                                                    "Diabetes", "Coronary Artery Disease")),
                                    
                                    hr(style = "border-color: #e5e7eb;"),
                                    
                                    tags$div(style = "background: #f5f3ff; padding: 15px; border-radius: 8px; margin-top: 20px; border-left: 4px solid #8b5cf6;",
                                             h5("How to Read:", style = "color: #6d28d9; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               strong("Box indicates:"), " Middle 50% of patients", br(), br(),
                                               strong("Line in box:"), " Median (middle value)", br(), br(),
                                               strong("Dots:"), " Individual patient values", br(), br(),
                                               "Compare box positions to see how diagnoses differ!")
                                    ),
                                    
                                    hr(style = "border-color: #e5e7eb; margin-top: 20px;"),
                                    
                                    tags$div(style = "background: #dbeafe; padding: 15px; border-radius: 8px; border-left: 4px solid #3b82f6;",
                                             h5("What This Shows:", style = "color: #1e40af; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "See typical metric values for each diagnosis.", br(), br(),
                                               "Higher boxes = higher values for that condition.", br(), br(),
                                               "Use this to understand health profiles!")
                                    )
                           )
                    ),
                    
                    # Main visualization
                    column(9,
                           # Summary stats cards
                           fluidRow(
                             column(12,
                                    uiOutput("metric_summary_cards")
                             )
                           ),
                           
                           # Box plot
                           fluidRow(
                             column(12,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-top: 20px;",
                                             h4("Distribution Comparison", style = "color: #374151; margin-bottom: 15px;"),
                                             p("Each box shows the range and distribution of values for that diagnosis",
                                               style = "color: #6b7280; font-size: 14px; margin-bottom: 15px;"),
                                             plotOutput("diagnosis_boxplot", height = 500)
                                    )
                             )
                           )
                    )
                  )
         )
),
######################################

######################################
# 4 TREATMENT PATHWAYS:
tabPanel("Treatment Pathways",
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #10b981 0%, #059669 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Treatment Pathways Explorer", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Discover how different diagnoses are treated and explore patient care patterns",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         # Main content
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  
                  # Filter section
                  fluidRow(
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                    h4("Explore By:", style = "color: #10b981; margin-bottom: 20px;"),
                                    
                                    selectInput("treatment_diagnosis",
                                                "Diagnosis:",
                                                choices = c("All Diagnoses", unique(healthcare_dataset$Diagnosis)),
                                                selected = "All Diagnoses"),
                                    
                                    hr(style = "border-color: #e5e7eb;"),
                                    
                                    checkboxGroupInput("treatment_types",
                                                       "Show Treatments:",
                                                       choices = c("Surgery", "Medication", "Lifestyle Changes", "Observation", "None"),
                                                       selected = c("Surgery", "Medication", "Lifestyle Changes", "Observation", "None")),
                                    
                                    tags$div(style = "background: #f0fdf4; padding: 15px; border-radius: 8px; margin-top: 20px; border-left: 4px solid #10b981;",
                                             h5("Quick Guide:", style = "color: #059669; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "🔴 Surgery - Procedures", br(),
                                               "🔵 Medication - Drugs", br(),
                                               "🟢 Lifestyle - Diet/Exercise", br(),
                                               "🟠 Observation - Monitoring", br(),
                                               "⚪ None - No treatment")
                                    )
                           )
                    ),
                    
                    # Main visualizations
                    column(9,
                           # Single large visualization for treatment breakdown
                           fluidRow(
                             column(12,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-bottom: 20px;",
                                             h4("Treatment Breakdown", style = "color: #374151; margin-bottom: 15px;"),
                                             plotOutput("treatment_pie", height = 450)
                                    )
                             )
                           ),
                           
                           # Stats section below
                           fluidRow(
                             column(12,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                             h4("Quick Stats", style = "color: #374151; margin-bottom: 15px;"),
                                             htmlOutput("treatment_stats")
                                    )
                             )
                           )
                    )
                  )
         )
),
##############################################


######################################
# 5 HEALTH RISK CALCULATOR:
tabPanel("Health Risk Calculator",
         # Standardized hero header
         tags$div(style = "background: linear-gradient(135deg, #d32f2f 0%, #b71c1c 100%); padding: 50px 20px; text-align: center; color: white;",
                  h1("Health Risk Calculator", 
                     style = "font-size: 38px; font-weight: bold; margin-bottom: 15px;"),
                  p("Get personalized health assessments and compare with patient data",
                    style = "font-size: 18px; opacity: 0.95; max-width: 700px; margin: 0 auto;")
         ),
         
         tags$div(style = "max-width: 1400px; margin: 40px auto; padding: 0 20px;",
                  fluidRow(
                    # LEFT SIDEBAR - Input Panel
                    column(3,
                           tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); height: 100%;",
                                    h4("Enter Your Health Metrics:", style = "color: #d32f2f; margin-bottom: 20px;"),
                                    
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
                                    
                                    hr(style = "border-color: #e5e7eb; margin: 25px 0;"),
                                    
                                    actionButton("calculate_risk",
                                                 "Calculate My Risk Profile",
                                                 style = "background-color: #d32f2f; color: white; font-weight: bold; width: 100%; padding: 12px; font-size: 16px; border: none; border-radius: 6px; cursor: pointer;"),
                                    
                                    hr(style = "border-color: #e5e7eb; margin: 25px 0;"),
                                    
                                    tags$div(style = "background: #ffebee; padding: 15px; border-radius: 8px; border-left: 4px solid #d32f2f;",
                                             h5("About This Tool:", style = "color: #b71c1c; margin-top: 0;"),
                                             p(style = "font-size: 12px; color: #374151; margin: 5px 0; line-height: 1.6;",
                                               "• Compare your metrics to our patient database", br(),
                                               "• See your percentile rankings", br(),
                                               "• Understand your risk profile", br(), br(),
                                               tags$strong("Note:"), " This is for informational purposes only. Always consult a healthcare provider.")
                                    )
                           )
                    ),
                    
                    # RIGHT PANEL - Results
                    column(9,
                           # Risk Level Box
                           tags$div(style = "margin-bottom: 20px;",
                                    uiOutput("risk_level_box")
                           ),
                           
                           # Percentile Rankings and Similar Patients
                           fluidRow(
                             column(6,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); height: 100%;",
                                             h4("Your Percentile Rankings", style = "color: #374151; margin-bottom: 15px;"),
                                             p("See how your metrics compare to all patients in our database",
                                               style = "color: #6b7280; font-size: 14px; margin-bottom: 15px;"),
                                             htmlOutput("percentile_rankings")
                                    )
                             ),
                             column(6,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); height: 100%;",
                                             h4("Similar Patient Profile", style = "color: #374151; margin-bottom: 15px;"),
                                             p("Patients with similar age, gender, and BMI",
                                               style = "color: #6b7280; font-size: 14px; margin-bottom: 15px;"),
                                             htmlOutput("similar_patients_info")
                                    )
                             )
                           ),
                           
                           # Diagnosis Distribution Chart
                           fluidRow(
                             column(12,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-top: 20px;",
                                             h4("Common Diagnoses for Similar Patients", style = "color: #374151; margin-bottom: 15px;"),
                                             p("This chart shows the most frequent diagnoses among patients in our dataset with similar characteristics to yours (within ±10 years age, same gender, and ±5 BMI points). The percentages indicate what proportion of these similar patients received each diagnosis.",
                                               style = "color: #6b7280; font-size: 14px; margin-bottom: 10px; line-height: 1.5;"),
                                             tags$div(style = "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800; margin-bottom: 20px;",
                                                      p(style = "margin: 0; font-size: 13px; color: #e65100; line-height: 1.5;",
                                                        tags$strong("Important Note:"), " This dataset only includes individuals who sought medical care at healthcare facilities. Many healthy individuals with similar profiles who did not require medical attention are not represented in this data. These percentages should not be interpreted as your likelihood of developing these conditions.")
                                             ),
                                             plotOutput("diagnosis_distribution", height = 300)
                                    )
                             )
                           ),
                           
                           # Comparison Chart
                           fluidRow(
                             column(12,
                                    tags$div(style = "background: white; padding: 25px; border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-top: 20px;",
                                             h4("How You Compare to Dataset Averages", style = "color: #374151; margin-bottom: 15px;"),
                                             p("This comparison shows your health metrics (in red) side-by-side with the average values from our entire patient dataset (in blue). This helps you see where your values are higher or lower than typical patients in our database.",
                                               style = "color: #6b7280; font-size: 14px; margin-bottom: 15px; line-height: 1.5;"),
                                             plotOutput("comparison_chart", height = 350)
                                    )
                             )
                           )
                    )
                  )
         )
)
##############################################

) # connects to navbarPage at top! must engulf WHOLE THING!