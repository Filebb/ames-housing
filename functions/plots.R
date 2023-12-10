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

# FREQUENCY CHART FOR CATEGORICAL VARIABLES
nominal_frequency_bar <- function(data, nominal, nominal_label, title = NULL){
  
  max_count <- data |> 
    count({{ nominal }}) |> 
    summarise(max(n)) |> 
    pull()
  
  data |> 
    
    ggplot(mapping = aes(y = fct_rev({{ nominal }}))) +
    
    geom_bar(stat = "count", col = "black", fill = "dodgerblue2") +
    
    stat_count(geom = "text",
               
               aes(label = paste0(sprintf("%0.1f", after_stat(count)/sum(after_stat(count)) * 100), "%")),
               
               hjust = -0.1, size = 4) +
    
    labs(title = title,
         x     = "Count",
         y     = nominal_label) +
    
    scale_x_continuous(limits = c(0, max_count * 1.2))
}

# BOX PLOT NUMERIC VS CATEGORICAL
nominal_numeric_box <- function(data, nominal, numeric,
                                nominal_label = NULL,
                                numeric_label = NULL,
                                title = NULL) {
  
  v_median <- data |> 
    summarise(median( {{ numeric }} , na.rm = TRUE)) |> 
    pull()
  
  data |> 
    
    ggplot(mapping = aes(x = {{ numeric }}, y = fct_reorder( {{ nominal }}, {{ numeric }} ))) +
    
    geom_boxplot(fill = "dodgerblue2", outlier.shape = NA) +
    
    stat_boxplot(geom = "errorbar", width = 0.4) +
    
    geom_jitter(color = "black", alpha = .03, width = .2) +
    
    geom_vline(mapping = aes(xintercept = v_median, col = "median"),
               linewidth = 1) +
    
    scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    
    labs(title = title,
         x     = numeric_label,
         y     = nominal_label) +
    
    scale_color_manual(values = c("median" = "tomato2")) +
    
    theme(legend.position = c(0.8, 0.1),
          legend.title    = element_blank(),
          legend.text     = element_text(size = 12))
}

# COMBINED FREQUENCY CHART AND BOX PLOT
nominal_vs_numeric <- function(data, nominal, numeric, nominal_label,
                               numeric_label, title = NULL) {
  nominal_frequency_bar(data, {{ nominal }}, nominal_label) +
    nominal_numeric_box(data, {{ nominal }}, {{ numeric }}, numeric_label) +
    plot_annotation(title = title,
                    theme = theme(plot.title = element_text(hjust = 0.5,
                                                            size = 28)))
  
}

# NUMERIC VS NUMERIC
scatter_plot <- function(data, x_numeric, y_numeric,
                         x_label, y_label, title){
  
  data %>%
    
    ggplot(mapping = aes(x = {{ x_numeric}}, y = {{ y_numeric}})) +
    
    geom_point() +
    
    geom_smooth(method = "lm", col = "dodgerblue3", fill = "grey70", linewidth = 1.1) +
    
    scale_x_continuous(label = label_number(scale_cut = cut_short_scale())) +
    
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    
    xlab(x_label) +
    
    ylab(y_label) +
    
    ggtitle(title)
  
}