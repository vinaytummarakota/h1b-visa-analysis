library(docstring)
library(tidyverse)
library(glue)
library(scales)

identify_top_n_categories <- function(df, segment_col, segment_name, num_categories, label_mapping) {
  #' @description: Identifies the top-N values of {segment_col} based on applicant volume
  #' 
  #' @param df: The dataframe containing the H-1B results
  #' @param segment_col: A string containing the name of the column along which outcomes should be calculated 
  #' @param segment_name: A string containing the plain language name of the column along which outcomes should be calculated
  #' @param num_categories: An integer representing the # of categories we wish to include in the resulting dataframe (based on the N most frequent categories in the dataset)
  #' @param label_mapping: A list mapping the values in a given column to more readable values; also maps
  #' name changes to the same group (for the highest volume groups only)
  
  top_n_categories <- df %>%
    mutate(!!as.symbol(segment_col) := recode(!!as.symbol(segment_col), !!!label_mapping)) %>%
    group_by(!!as.symbol(segment_col)) %>%
    summarise(num_obs = n()) %>%
    arrange(desc(num_obs)) %>%
    top_n(num_categories) %>%
    pull(!!as.symbol(segment_col))
  
  return(top_n_categories)
}

estimate_outcome_rates <- function(df, segment_col, segment_name, outcome_col, outcome_name, top_n_categories, label_mapping) {
  #' @description: Estimates the rate of {outcome_col} by {segment_col} across lottery years
  #'  
  #' @param df: The dataframe containing the H-1B results
  #' @param segment_col: A string containing the name of the column along which outcomes should be calculated 
  #' @param segment_name: A string containing the plain language name of the column along which outcomes should be calculated
  #' @param outcome_col: A string containing the name of the column containing the outcome of interest
  #' @param outcome_name: A string containing the plain language name of the column containing the outcome of interest
  #' @param top_n_categories: The top-N values of {segment_col} by applicant volume
  #' @param label_mapping: A list mapping the values in a given column to more readable values; also maps
  #' name changes to the same group (for the highest volume groups only)
  
  outcome_df <- df %>%
    mutate(!!as.symbol(segment_col) := recode(!!as.symbol(segment_col), !!!label_mapping)) %>%
    filter(!!as.symbol(segment_col) %in% top_n_categories) %>%
    group_by(lottery_year, !!as.symbol(segment_col)) %>%
    summarise(outcome_rate = sum(!!as.symbol(outcome_col))/n())
  
  return(outcome_df)
}

bump_plot <- function(df, segment_col, segment_name, outcome_col, outcome_name, num_categories, label_mapping, title) {
  #' @description: Estimates the rate of {outcome_col} by {segment_col} across lottery years and plots the results using a bump plot
  #' 
  #' @param df: The dataframe containing the H-1B results
  #' @param segment_col: A string containing the name of the column along which outcomes should be calculated 
  #' @param segment_name: A string containing the plain language name of the column along which outcomes should be calculated
  #' @param outcome_col: A string containing the name of the column containing the outcome of interest
  #' @param outcome_name: A string containing the plain language name of the column containing the outcome of interest
  #' @param num_categories: An integer representing the # of categories we wish to include in this visualization (based on the N most frequent categories in the dataset)
  #' @param label_mapping: A list mapping the values in a given column to more readable values; also maps
  #' name changes to the same group (for the highest volume groups only)
  
  top_n_categories <- identify_top_n_categories(df, segment_col, segment_name, num_categories, label_mapping)
  
  rank_df <- estimate_outcome_rates(df, segment_col, segment_name, outcome_col, outcome_name, top_n_categories, label_mapping) %>%
    arrange(lottery_year, desc(outcome_rate)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= num_categories) %>%
    mutate(!!as.symbol(segment_col) := fct_reorder(!!as.symbol(segment_col), rank))
  
  ggplot(rank_df, aes(x = lottery_year, y = rank, group = !!as.symbol(segment_col), color = !!as.symbol(segment_col))) +
    geom_line(size = 2) +
    geom_point(size = 6) +
    geom_text(aes(label = rank), vjust = 0.6, size = 3.5, color = 'white') +
    scale_y_reverse(breaks = 1:nrow(df)) +
    scale_color_discrete(name = segment_name)+
    labs(x = "Lottery Year (FY)", y = glue("Rank (Based on {outcome_name})"), title = title, subtitle = glue("Sample is limited to top {num_categories} {str_to_lower(segment_name)} by application volume"), caption = "Visualization: Vinay Tummarakota, @unboxpolitics\nSource: USCIS (Obtained by Bloomberg)") +
    theme_minimal()
}

line_plot_categorical <- function(df, segment_col, segment_name, outcome_col, outcome_name, num_categories, label_mapping, title) {
  #' @description: Estimates the rate of {outcome_col} by {segment_col} across lottery years and plots the results using a line plot
  #' 
  #' @param df: The dataframe containing the H-1B results
  #' @param segment_col: A string containing the name of the column along which outcomes should be calculated 
  #' @param segment_name: A string containing the plain language name of the column along which outcomes should be calculated
  #' @param outcome_col: A string containing the name of the column containing the outcome of interest
  #' @param outcome_name: A string containing the plain language name of the column containing the outcome of interest
  #' @param num_categories: An integer representing the # of categories we wish to include in this visualization (based on the N most frequent categories in the dataset)
  #' @param label_mapping: A list mapping the values in a given column to more readable values; also maps
  #' name changes to the same group (for the highest volume groups only)
  
  top_n_categories <- identify_top_n_categories(df, segment_col, segment_name, num_categories, label_mapping)
  
  rate_df <- estimate_outcome_rates(df, segment_col, segment_name, outcome_col, outcome_name, top_n_categories, label_mapping) %>%
    mutate(!!as.symbol(segment_col) := fct_reorder(!!as.symbol(segment_col), desc(outcome_rate)))
  
  ggplot(rate_df, aes(x = lottery_year, y = outcome_rate, group = !!as.symbol(segment_col), col = !!as.symbol(segment_col)))+
    geom_line()+
    geom_point()+
    labs(x = "Lottery Year (FY)", y = outcome_name, title = title, caption = "Visualization: Vinay Tummarakota, @unboxpolitics\nSource: USCIS (Obtained by Bloomberg)")+
    scale_y_continuous(labels = percent_format())+
    scale_color_discrete(name = segment_name)+
    theme_minimal()
  
}

line_plot_continuous <- function(df, segment_col, segment_name, outcome_col, outcome_name, min_val, max_val, title) {
  #' @description: Estimates the rate of {outcome_col} by {segment_col} across lottery years and 
  #' plots the results using a line plot
  #' 
  #' @param df: The dataframe containing the H-1B results
  #' @param segment_col: A string containing the name of the continuous-valued column along which outcomes should be calculated 
  #' @param segment_name: A string containing the plain language name of the continuous-valued column along which outcomes should be calculated
  #' @param outcome_col: A string containing the name of the column containing the outcome of interest
  #' @param outcome_name: A string containing the plain language name of the column containing the outcome of interest
  #' @param min_val: The lowest possible value in {segment_col} that should be plotted
  #' @param max_val: The highest possible value in {segment_col} that should be plotted
  #' @param title: A string containing the title of the graph
  
  agg_df <- df %>%
    group_by(lottery_year, !!as.symbol(segment_col)) %>%
    summarise(outcome_rate = sum(!!as.symbol(outcome_col))/n()) %>%
    mutate(lottery_year = as.factor(lottery_year)) %>%
    filter(!is.na(lottery_year))
  
  ggplot(agg_df, aes(x = !!as.symbol(segment_col), y = outcome_rate, group = lottery_year, col = lottery_year))+
    geom_line()+
    xlim(min_val, max_val)+
    labs(x = "Age (Years)", y = glue("{outcome_name} Rate"), title = title, subtitle = glue("Sample is limited to {min_val}-{max_val} year olds"), caption = "Visualization: Vinay Tummarakota, @unboxpolitics\nSource: USCIS (Obtained by Bloomberg)")+
    scale_y_continuous(labels = percent_format())+
    scale_color_discrete("Lottery Year (FY)")+
    theme_minimal()
}
