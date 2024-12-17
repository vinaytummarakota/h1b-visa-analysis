library(docstring)
library(readxl)
library(janitor)
library(tidyverse)

clean_h1b_df <- function(path, winning_statuses) {
  #' @description: Cleans the raw H1-B spreadsheet data and saves the result in the {clean_data} folder
  #' 
  #' @param path: A string representing the path to the dataframe that needs to be cleaned
  
  df <- read.csv(path)
  
  cleaned_df <- df %>%
    clean_names() %>%
    select(country_of_nationality, ben_year_of_birth, employer_name, lottery_year, status_type, 
           ben_multi_reg_ind, ben_education_code) %>%
    mutate(reg_id = row_number(), 
           age_in_years = as.integer(lottery_year) - as.integer(ben_year_of_birth),
           lottery_year = as.integer(lottery_year), 
           won_lottery = ifelse(is.na(status_type), NA, status_type == "SELECTED"), 
           had_multiple_regs = as.integer(ben_multi_reg_ind) == 1, 
           has_grad_degree = ifelse(is.na(ben_education_code), NA, ben_education_code %in% c("G", "H", "I"))) %>%
    rename(country_code = country_of_nationality) %>%
    select(-c(ben_year_of_birth, status_type, ben_education_code))
  
  return(cleaned_df)
}