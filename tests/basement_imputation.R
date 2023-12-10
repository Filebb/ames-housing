library(tidyverse)

ames <- read_rds(file = "data/ames-renamed.RDS")

# number of missing values across basement variables
ames |> 
    select(contains("basement")) |> 
    map_int(~sum(is.na(.)))

# missing values appear most of the times when
# basement_total_area = 0 (the I interpret as no basement)
ames |> 
    select(contains("basement")) |> 
    filter(basement_total_area == 0) |> 
    map_int(~sum(is.na(.)))

# we can impute the nominal variables of these cases with "No Basement"
# and the numeric variables with 0
ames |> 
    select(contains("basement")) |> 
    mutate(
        basement_quality         = if_else(basement_total_area == 0, replace_na(basement_quality,         "No Basement"), basement_quality),
        basement_condition       = if_else(basement_total_area == 0, replace_na(basement_condition,       "No Basement"), basement_condition),
        basement_exposure        = if_else(basement_total_area == 0, replace_na(basement_exposure,        "No Basement"), basement_exposure),
        basement_finished_type_1 = if_else(basement_total_area == 0, replace_na(basement_finished_type_1, "No Basement"), basement_finished_type_1),
        basement_finished_type_2 = if_else(basement_total_area == 0, replace_na(basement_finished_type_2, "No Basement"), basement_finished_type_2),
        basement_full_bathrooms  = if_else(basement_total_area == 0, replace_na(basement_full_bathrooms,  0),             basement_full_bathrooms),
        basement_half_bathrooms  = if_else(basement_total_area == 0, replace_na(basement_half_bathrooms,  0),             basement_half_bathrooms)
    ) |> 
    map_int(~sum(is.na(.)))

# what remains? let's have a look
ames |> 
    select(id, contains("basement")) |> 
    mutate(
        basement_quality         = if_else(basement_total_area == 0, replace_na(basement_quality,         "No Basement"), basement_quality),
        basement_condition       = if_else(basement_total_area == 0, replace_na(basement_condition,       "No Basement"), basement_condition),
        basement_exposure        = if_else(basement_total_area == 0, replace_na(basement_exposure,        "No Basement"), basement_exposure),
        basement_finished_type_1 = if_else(basement_total_area == 0, replace_na(basement_finished_type_1, "No Basement"), basement_finished_type_1),
        basement_finished_type_2 = if_else(basement_total_area == 0, replace_na(basement_finished_type_2, "No Basement"), basement_finished_type_2),
        basement_full_bathrooms  = if_else(basement_total_area == 0, replace_na(basement_full_bathrooms,  0),             basement_full_bathrooms),
        basement_half_bathrooms  = if_else(basement_total_area == 0, replace_na(basement_half_bathrooms,  0),             basement_half_bathrooms)
    ) |> 
    filter(if_any(everything(), is.na)) |> 
    view()

# when basement_total_area is NA then I assume "No Basement"
# (or 0 for numerica variables)
ames |> 
    select(id, contains("basement")) |> 
    mutate(
        basement_quality         = if_else(basement_total_area %in% c(0, NA), replace_na(basement_quality,         "No Basement"), basement_quality),
        basement_condition       = if_else(basement_total_area %in% c(0, NA), replace_na(basement_condition,       "No Basement"), basement_condition),
        basement_exposure        = if_else(basement_total_area %in% c(0, NA), replace_na(basement_exposure,        "No Basement"), basement_exposure),
        basement_finished_type_1 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_1, "No Basement"), basement_finished_type_1),
        basement_area_type_1     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_1,     0),             basement_area_type_1),
        basement_finished_type_2 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_2, "No Basement"), basement_finished_type_2),
        basement_area_type_2     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_2,     0),             basement_area_type_2),
        basement_unfinished_area = if_else(basement_total_area %in% c(0, NA), replace_na(basement_unfinished_area, 0),             basement_unfinished_area),
        basement_total_area      = if_else(basement_total_area %in% c(0, NA), replace_na(basement_total_area,      0),             basement_total_area),
        basement_full_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_full_bathrooms,  0),             basement_full_bathrooms),
        basement_half_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_half_bathrooms,  0),             basement_half_bathrooms)
    ) |> 
    filter(if_any(everything(), is.na)) |> 
    map_int(~sum(is.na(.)))

# for the remaining cases I impute "No" for basement_exposure
# and "Unf" for basement_finished_type_2
ames |> 
    select(id, contains("basement")) |> 
    mutate(
        basement_quality         = if_else(basement_total_area %in% c(0, NA), replace_na(basement_quality,         "No Basement"), basement_quality),
        basement_condition       = if_else(basement_total_area %in% c(0, NA), replace_na(basement_condition,       "No Basement"), basement_condition),
        basement_exposure        = if_else(basement_total_area %in% c(0, NA), replace_na(basement_exposure,        "No Basement"), basement_exposure),
        basement_finished_type_1 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_1, "No Basement"), basement_finished_type_1),
        basement_area_type_1     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_1,     0),             basement_area_type_1),
        basement_finished_type_2 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_2, "No Basement"), basement_finished_type_2),
        basement_area_type_2     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_2,     0),             basement_area_type_2),
        basement_unfinished_area = if_else(basement_total_area %in% c(0, NA), replace_na(basement_unfinished_area, 0),             basement_unfinished_area),
        basement_total_area      = if_else(basement_total_area %in% c(0, NA), replace_na(basement_total_area,      0),             basement_total_area),
        basement_full_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_full_bathrooms,  0),             basement_full_bathrooms),
        basement_half_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_half_bathrooms,  0),             basement_half_bathrooms)
    ) |> 
    filter(if_any(everything(), is.na)) |> 
    mutate(
        basement_exposure        = replace_na(basement_exposure, "No"),
        basement_finished_type_2 = replace_na(basement_finished_type_2, "Unf")
    ) |> 
    view()

# script for imputation
ames |> 
    select(id, contains("basement")) |> 
    mutate(
        basement_quality         = if_else(basement_total_area %in% c(0, NA), replace_na(basement_quality,         "No Basement"), basement_quality),
        basement_condition       = if_else(basement_total_area %in% c(0, NA), replace_na(basement_condition,       "No Basement"), basement_condition),
        basement_exposure        = if_else(basement_total_area %in% c(0, NA), replace_na(basement_exposure,        "No Basement"), basement_exposure),
        basement_finished_type_1 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_1, "No Basement"), basement_finished_type_1),
        basement_area_type_1     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_1,     0),             basement_area_type_1),
        basement_finished_type_2 = if_else(basement_total_area %in% c(0, NA), replace_na(basement_finished_type_2, "No Basement"), basement_finished_type_2),
        basement_area_type_2     = if_else(basement_total_area %in% c(0, NA), replace_na(basement_area_type_2,     0),             basement_area_type_2),
        basement_unfinished_area = if_else(basement_total_area %in% c(0, NA), replace_na(basement_unfinished_area, 0),             basement_unfinished_area),
        basement_total_area      = if_else(basement_total_area %in% c(0, NA), replace_na(basement_total_area,      0),             basement_total_area),
        basement_full_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_full_bathrooms,  0),             basement_full_bathrooms),
        basement_half_bathrooms  = if_else(basement_total_area %in% c(0, NA), replace_na(basement_half_bathrooms,  0),             basement_half_bathrooms),
        
        basement_exposure        = replace_na(basement_exposure, "No"),
        basement_finished_type_2 = replace_na(basement_finished_type_2, "Unf")
    )
