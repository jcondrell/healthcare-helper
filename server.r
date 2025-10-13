library(shiny)   # shiny comes with the sidebar page ... where you use the render calls
library(tidyverse)
library(ggplot2)


# healthcare_dataset <- read_csv("healthcare_dataset.csv")
# stroke_prediction <- read_csv("stroke prediction.csv")

data <- readRDS("datasets.RDS")
healthcare_dataset <- data$healthcare_dataset
stroke_prediction <- data$stroke_prediction

function(input, output) {}