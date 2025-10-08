library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)

healthcare_dataset <- read_csv("healthcare_dataset.csv")
stroke_prediction <- read_csv("stroke prediction.csv")