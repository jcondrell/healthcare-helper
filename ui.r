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
  )
##############################################


) # connects to navbarPage at top! must engulf WHOLE THING!
