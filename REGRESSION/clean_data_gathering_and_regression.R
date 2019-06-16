library(tidyverse) 
library(lubridate)
library(dplyr)
# setwd("C:/Users/mjg07/OneDrive/Documents/MDSI/36103 Statistical Thinking for Data Science/Assignment 2")

###################################
# Functions to setup data imports #
###################################

get_clean_rent_data_by_LGA_year <- function() {
  rent <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv')
  rent <- (rent %>% 
             rowwise() %>% mutate(year = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000) %>% 
             mutate(X1 = NULL, Quarter = NULL) %>% 
             filter(year >= 2011 & year <= 2016))
  
  rent <- rent %>% mutate(Rent_1_bedroom = as.numeric(Rent_1_bedroom), 
                          Rent_2_bedroom = as.numeric(Rent_2_bedroom),
                          Rent_3_bedroom = as.numeric(Rent_3_bedroom),
                          Rent_4_bedroom = as.numeric(Rent_4_bedroom))

  #TODO get the Average rent across state  
  
  
  return (rent %>% group_by(LGA, year) %>% summarize(Average_rent_1_bedroom = mean(Rent_1_bedroom, na.rm = TRUE),
                                                     Average_rent_2_bedroom = mean(Rent_2_bedroom, na.rm = TRUE),
                                                    Average_rent_3_bedroom = mean(Rent_3_bedroom, na.rm = TRUE),
                                                    Average_rent_4_bedroom = mean(Rent_4_bedroom, na.rm = TRUE)))
}

get_clean_median_income_by_LGA_year <-  function() {
  return (median_income <- read_csv('STD_AT2/CLEAN DATA/Income.csv') 
   %>% filter(YEAR <= 2016) %>% rename('year' = 'YEAR') %>% mutate(Median_income= as.numeric(`Median Employee income $`)) %>%
   mutate(`Median Employee income $`= NULL))
}

get_offence_population_by_LGA_year <-  function() {
  return(offence_pop <- read_csv('STD_AT2/CLEAN DATA/offence_data_EDA.csv') %>% rename('year' = 'Year') %>%
     mutate(Population_Density = as.numeric(Population_Density)))
  
}

get_business_entries_rate_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/Business.csv') %>% select(LGA, year, New_business_entries_rate))
}

get_hostpitalisation_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/alcohol_hosp_death.csv'))
}

get_frequency_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/alcohol_freq_LGA.csv'))
}

get_age_by_LGA_year <- function() {
  return(read_csv('STD_AT2/CLEAN DATA/Population_Clean.csv') %>% 
     select(LGA,Year,`Working Age Population (15-64 years)`,Male_Median_Age,Female_Median_Age,Person_Median_Age) %>%
     rename('year' = 'Year') %>% rename('Working_Age_Population_15_64_years' = `Working Age Population (15-64 years)`) %>%
    mutate(Working_Age_Population_15_64_years = as.numeric(Working_Age_Population_15_64_years)) %>%
     mutate(Male_Median_Age = as.numeric(Male_Median_Age)) %>%
     mutate(Female_Median_Age = as.numeric(Female_Median_Age)) %>%
     mutate(Person_Median_Age = as.numeric(Person_Median_Age))
   )
 
}

get_unemployment_rate_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/Unemployment.csv') %>%
    rename('year' = 'YEAR') %>% mutate(Unemployment_rate = as.numeric(Unemployment_rate)))
}

get_bachelor_degree_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/Bachelor_Degree.csv') %>%
            rename('year' = 'YEAR') %>% mutate(Bachelor_Degree = as.numeric(Bachelor_Degree)))
}

get_data_for_spatial_analysis <- function(raw.data) {
  new.data <- raw.data %>% filter(year=='2016') %>% mutate_if(is.character, str_to_upper) %>% mutate('year' = NULL)
  new.data$LGA[new.data$LGA == "UNINCORPORATED NSW"] <- "UNINCORPORATED"
  return (new.data)
}



# Import the data

rent <- get_clean_rent_data_by_LGA_year()

income <- get_clean_median_income_by_LGA_year()

offence_pop <- get_offence_population_by_LGA_year()

(hospitalisation <- get_hostpitalisation_by_LGA_year());

business <- get_business_entries_rate_by_LGA_year();

frequency <- get_frequency_by_LGA_year();

age <- get_age_by_LGA_year();

unemployment_rate <- get_unemployment_rate_by_LGA_year();

bachelor_degree = get_bachelor_degree_by_LGA_year();

# Join  all the data sets together
joined_data <- inner_join(inner_join(inner_join(inner_join(inner_join(rent, income, by = c('LGA','year')), offence_pop, by = c('LGA', 'year')), business, by = c('LGA', 'year')), hospitalisation, by = c('LGA', 'year')), frequency, by = c('LGA', 'year'))
# Joining all the data we end up with a very limited data set (down from 1200+ observations down to only 145....)
