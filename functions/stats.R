library(tidyverse)
library(moments)

library(gt)

# NUMERICAL SUMMARIES FOR NUMERIC VARIABLES (output: tibble)
numeric_summary <- function(x) {
  tibble(statistic = "Minimum",
         value     = min(x, na.rm = TRUE)) |>
    
    add_row(statistic = "1st Quartile (Q1)",
            value     = quantile(x, probs = 0.25, na.rm = TRUE)) |>
    
    add_row(statistic = "Median (Q2)",
            value     = median(x, na.rm = TRUE)) |>
    
    add_row(statistic = "Mean",
            value     = mean(x, na.rm = TRUE)) |>
    
    add_row(statistic = "3rd Quartile (Q3)",
            value     = quantile(x, probs = 0.75, na.rm = TRUE)) |>
    
    add_row(statistic = "Maximum",
            value     = max(x, na.rm = TRUE)) |>
    
    add_row(statistic = "Range",
            value     = diff(range(x, na.rm = TRUE))) |>
    
    add_row(statistic = "Standard Deviation",
            value     = sd(x, na.rm = TRUE)) |>
    
    add_row(statistic = "Interquartile Range (IQR)",
            value     = IQR(x, na.rm = TRUE)) |>
    
    add_row(statistic = "Coefficient of Variation (CV)",
            value     = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) |>
    
    add_row(statistic = "Skewness",
            value     = skewness(x, na.rm = TRUE)) |>
    
    add_row(statistic = "NAs",
            value     = sum(is.na(x)))
}

# NUMERICAL SUMMARIES TABLE (output: gt table)
numeric_summary_table <- function(data, numeric) {
  data |>
    reframe(numeric_summary( {{ numeric }} )) |> 
    gt() |> 
    fmt_number(columns = 2) |> 
    fmt_number(columns = 2, rows = 12, decimals = 0) |> 
    fmt_percent(columns = 2, rows = 10, decimals = 2) |> 
    cols_label(statistic = "Statistic",
               value     = "Value") |> 
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_column_labels())
}

# FREQUENCY TABLE FOR CATEGORICAL VARIABLE (output: tibble)
nominal_frequency <- function(data, nominal) {
  
  data |>
    count({{ nominal }}, name = "freq") |>
    mutate(rel_freq = freq / sum(freq))
  
}

# NUMERICAL SUMMARIES TABLE (output: gt table)
nominal_frequency_table <- function(data, nominal) {
  data |>
    count({{ nominal }}, name = "freq") |>
    mutate(rel_freq = freq / sum(freq)) |>  
    gt() |> 
    fmt_percent(columns = 3, decimals = 2) |> 
    cols_align(columns = 1, align = "left") |> 
    cols_label(freq     = "Frequency",
               rel_freq = "(%)") |> 
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_column_labels())
}