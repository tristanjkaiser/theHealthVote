# U.S. Chronic Disease Indicators (CDI)
# By: Tristan Kaiser
# 12/1/2016

library(stringr)
library(tidyverse)
library(plotly)
library(RColorBrewer)

setwd("data")

# Plotly details

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))

#-------------------

df <- read_csv("U.S._Chronic_Disease_Indicators__CDI_.csv") %>%
  mutate(LocationAbbr = paste("US-", LocationAbbr, sep=""))

# See list of questions
questions <- df %>%
  distinct(Question, QuestionID)

# Drill down to Cancer data
cancer.topics <- df %>%
  filter(Topic == "Cancer",
         str_detect(Question, "incidence"),
         DataValueUnit == "per 100,000",
         DataValueTypeID == "AvgAnnAgeAdjRate")

# All cancer incidence
allCancer <- cancer.topics %>%
  filter(QuestionID == "CAN4_1",
         LocationAbbr != "DC",
         LocationAbbr != "US") %>%
  mutate(DataValue = as.numeric(DataValue))

# Map Cancer incidence across US
p <- plot_geo(allCancer, locationmode = 'USA-states') %>%
  add_trace(
    z = ~DataValue, 
    color = ~DataValue, 
    colors = c("white", "#ef3b2c", "#67000d"),
    text = ~LocationDesc, 
    locations = ~LocationAbbr, 
    marker = list(line = l)
  ) %>%
  colorbar(title = 'Incidence per 100,000') %>%
  layout(
    title = 'Melanoma Incidence in the United States <br> Source: Data.gov',
    geo = g
  )
p

