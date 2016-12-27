# theHealthVote
# By: Tristan Kaiser
# 12/27/2016

library(choroplethr)
library(choroplethrMaps)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(magrittr)

#Data_Value	Description
#-9999	Indicate N.A. value from the source data for the Unemployed column on the VUNERABLEPOPSANDENVHEALTH page
#-2222 or -2222.2 or -2	nda, no data available, see Data Notes document for details
#-1111.1 or -1111 or -1	nrf, no report, see Data Notes document for details

#-------------------
# Load Summary Measures of Health, voting data, join, clean

fips_codes <- read_csv("data/fips_codes_website.csv") %>%
  mutate(County_FIPS_Code = `County FIPS Code`,
       State_FIPS_Code = `State FIPS Code`,
       combined_code =  paste(State_FIPS_Code, County_FIPS_Code, sep = ""))
       
fips_codes %<>% 
       mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
       State_FIPS_Code = as.numeric(State_FIPS_Code))
riskFactors <- read_csv("data/RISKFACTORSANDACCESSTOCARE.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

voteData <- read_csv("data/clean/Pres_Election_Data_2016i.csv") %>%
  dplyr::rename(County_FIPS_Code = COUNTY, State_FIPS_Code = STATEFIPS)

demographics <- read_csv("data/DEMOGRAPHICS.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

leadingCausesOfDeath <- read_csv("data/LEADINGCAUSESOFDEATH.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

relativeHealthImportance <- read_csv("data/LEADINGCAUSESOFDEATH.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))


# Need to modify combined_code to add to df, then use that as value for choropleth

df <- left_join(riskFactors, voteData, by = c("County_FIPS_Code", "State_FIPS_Code"))
df <- left_join(df, fips_codes, by = c("County_FIPS_Code", "State_FIPS_Code"))

df[df == "-1111.1"] <- NA
df[df == "-1111"] <- NA
df[df == "-2222"] <- NA
df[df == "-2222.2"] <- NA
df[df == "-9999"] <- NA
df[df == "-1"] <- NA
df[df == "-2"] <- NA

# First let's do some graphical representation of the vote
# Need to use the choroplethrMaps because plotly 
# does not support county-level data
# Must rename County_FIPS_Code to "region" and the value column "value"

#### Vote Map ####
df2 <- df
df2 %<>% dplyr::rename(region = combined_code)
df2 %<>% dplyr::rename(value = vote_difference)
df2 %<>% mutate(region = as.numeric(region))
df2 %<>% distinct(region, .keep_all = TRUE)
?county_choropleth
county_choropleth(df2,
                  title      = "2016 Presidential Vote Difference",
                  legend     = "Vote Difference",
                  num_colors = 7)

# Do Trump and Clinton voters eat their vegetables?
ggplot(df, aes(x = Few_Fruit_Veg, y = vote_difference)) +
  geom_point(aes(colour = victor)) +
  labs(x = "Few Fruits/Vegetables Consumed", y = "Vote differential (Trump = positive)") +
  theme_minimal()

#### Nearest Neighbors ####


