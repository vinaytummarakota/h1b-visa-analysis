library(docstring)
library(readxl)
library(janitor)
library(tidyverse)
library(stringr)

clean_h1b_df <- function(path, winning_statuses) {
  #' @description: Cleans the raw H1-B lottery registration data and saves the result in the {clean_data} folder
  #' 
  #' @param path: A string representing the path to the dataframe that needs to be cleaned
  
  df <- read.csv(path)
  
  cleaned_df <- df %>%
    clean_names() %>%
    select(country_of_nationality, ben_year_of_birth, city, state, employer_name, lottery_year, status_type, first_decision, 
           ben_multi_reg_ind, ben_education_code, rec_date, dot_code, naics_code, ben_comp_paid, job_title) %>%
    mutate(reg_id = row_number(), 
           age_in_years = as.integer(lottery_year) - as.integer(ben_year_of_birth),
           city = paste(city, state), 
           lottery_year = as.integer(lottery_year), 
           won_lottery = ifelse(is.na(status_type), NA, status_type == "SELECTED"), 
           had_multiple_regs = as.integer(ben_multi_reg_ind) == 1, 
           submitted_petition = ifelse(rec_date == "" | is.na(rec_date), FALSE, TRUE), 
           petition_approved = ifelse(first_decision == "" | is.na(first_decision), NA, first_decision == "Approved"), 
           dot_code = ifelse(dot_code == "" | is.na(dot_code), NA, sprintf("%03d", as.integer(dot_code))), 
           naics_code = ifelse(naics_code == "" | is.na(naics_code), NA, substr(naics_code, 1, 2)), 
           annual_salary = as.numeric(ben_comp_paid), 
           degree_level_is_missing = ben_education_code %in% c("", "J", "K", "R", NA), 
           has_grad_degree = ifelse(ben_education_code %in% c("", "J", "K", "R", NA), NA, ben_education_code %in% c("G", "H", "I"))) %>%
    rename(country_code = country_of_nationality) %>%
    select(-c(ben_year_of_birth, state, status_type, first_decision, ben_education_code, ben_comp_paid))
  
  return(cleaned_df)
}

clean_volume_df <- function(path, winning_statuses) {
  #' @description: Cleans the raw H1-B application volume data and saves the result in the {clean_data} folder
  #' 
  #' @param path: A string representing the path to the dataframe that needs to be cleaned
   
  df <- read.csv(path)
  
  colnames(df) <- c("lottery_year", "total_registrations", "eligible_registrations", "eligible_single_registrations", "eligible_multiple_registrations", "selected_registrations")
  
  cleaned_df <- df %>%
    select(c("lottery_year", "eligible_single_registrations", "eligible_multiple_registrations")) %>%
    pivot_longer(!lottery_year, names_to = "registration_type", values_to = "num_registrations") %>%
    mutate(num_registrations = as.integer(str_replace_all(num_registrations, "[*,]", "")), 
           registration_type := recode(registration_type, eligible_single_registrations = "Single Registrations", eligible_multiple_registrations = "Multiple Registrations"))
  
  return(cleaned_df)
}
