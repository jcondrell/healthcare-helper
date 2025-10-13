library(shiny)   # shiny comes with the sidebar page ... ui defines where outputs and inputs are on the webpage
library(tidyverse)
library(ggplot2)

# healthcare_dataset <- read_csv("healthcare_dataset.csv")
# state_facts <- read_csv("state_facts.csv")

data <- readRDS("new_datasets.RDS")
healthcare_dataset <- data$healthcare_dataset
state_facts <- data$state_facts