library(tidyverse)
library(patchwork)
library(ggthemes)
library(scales)

# SETUP OVERALL THEME
theme_set(
  theme_pander() +
    
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 13),
          axis.text  = element_text(size = 11, colour = "grey70")
    )
)

# DISTRIBUTION OF A NUMERIC VARIABLE
numeric_distribution <- function(data, numeric, xlabel, title) {
  
  v_iqr <- data |> 
    summarise(IQR( {{ numeric }} , na.rm = TRUE)) |> 
    pull()
  
  v_length <- data |> 
    summarise(length( {{ numeric }} )) |> 
    pull()
  
  v_mean <- data |> 
    summarise(mean( {{ numeric }} , na.rm = TRUE)) |> 
    pull()
  
  v_sd <- data |> 
    summarise(sd( {{ numeric }} , na.rm = TRUE)) |> 
    pull()
  
  # https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
  bin_width <- 2 * v_iqr / v_length^(1/3)
  
  data |> 
    ggplot(mapping = aes(x = {{ numeric }} )) +
    
    geom_histogram(binwidth = bin_width,
                   size     = 0.5,
                   col      = "white",
                   fill     = "orange2") +

    scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    
    labs(title = title,
         x     = xlabel,
         y     = "Count") +
    
    geom_hline(yintercept = 0,
               col        = "gray80",
               linewidth  = 0.5,
               linetype   = "dotted")
}
