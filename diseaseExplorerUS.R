# theHealthVote
# By: Tristan Kaiser
# 12/6/2016

library(stringr)
library(tidyverse)
library(plotly)
library(RColorBrewer)

#Data_Value	Description
#-9999	Indicate N.A. value from the source data for the Unemployed column on the VUNERABLEPOPSANDENVHEALTH page
#-2222 or -2222.2 or -2	nda, no data available, see Data Notes document for details
#-1111.1 or -1111 or -1	nrf, no report, see Data Notes document for details

#-------------------
# Load Summary Measures of Health, voting data, join, clean

riskFactors <- read_csv("data/RISKFACTORSANDACCESSTOCARE.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

voteData <- read_csv("data/clean/Pres_Election_Data_2016i.csv") %>%
  rename(County_FIPS_Code = COUNTY, State_FIPS_Code = STATEFIPS)

demographics <- read_csv("data/DEMOGRAPHICS.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

leadingCausesOfDeath <- read_csv("data/LEADINGCAUSESOFDEATH.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

relativeHealthImportance <- read_csv("data/LEADINGCAUSESOFDEATH.csv") %>%
  mutate(County_FIPS_Code = as.numeric(County_FIPS_Code),
         State_FIPS_Code = as.numeric(State_FIPS_Code))

df <- inner_join(riskFactors, voteData, by = c("County_FIPS_Code", "State_FIPS_Code"))

df[df == "-1111.1"] <- NA
df[df == "-1111"] <- NA
df[df == "-2222"] <- NA
df[df == "-2222.2"] <- NA
df[df == "-9999"] <- NA
df[df == "-1"] <- NA
df[df == "-2"] <- NA

# Do Trump and Clinton voters eat their vegetables?
ggplot(df, aes(x = Few_Fruit_Veg, y = vote_difference)) +
  geom_point(aes(colour = victor)) +
  labs(x = "Few Fruits/Vegetables Consumed", y = "Vote differential (Trump = positive)") +
  theme_minimal()