# PURPOSE: Clean and wrangle old abate data (pre 2021) and bind to new abate data
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: November 11, 2023
# NOTES:

library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

data_in <- "~/Github/dewormr/Data/pre_2021_data/abate"
df_geo_map_state <- read_xlsx("~/Github/dewormr/Data/dewormr_data_matching.xlsx") %>% filter(!is.na(state_join)) %>% select(state_join, state_fixed)
df_geo_map_county <- read_xlsx("~/Github/dewormr/Data/dewormr_data_matching.xlsx") %>% filter(!is.na(county_join)) %>% select(county_join, county_fixed)
files <- list.files(data_in, "*.xlsx", full.names = TRUE)
data_out <- "~/Github/dewormr/Dataout"

#function to stitch together historical files and standardize column names
stitch_abate <- function(file){
  
  sheetname <- readxl::excel_sheets(file) %>% 
    pluck(grep("20", .))
  
  df <- readxl::read_xlsx(file,
                          sheet = sheetname,
                          col_types = c("text")) %>%
    mutate(year = sheetname) %>% 
    dplyr::rename_all(~tolower(.)) %>% janitor::remove_empty() %>% mutate_all(trimws, which="both") %>% 
    clean_names() %>%
    rename_with(~gsub("census_", "", .), everything()) %>% 
    rename_with(~gsub("lat", "latitude", .), everything()) %>% 
    rename_with(~gsub("long", "longitude", .), everything()) %>% 
    rename_with(~gsub("amount_of_", "volume_of_", .), everything()) %>% 
    rename_with(~gsub("name_of_targeted_h3_water_source", "name_of_water_source", .), everything()) %>% 
    rename_with(~gsub("name_of_unsafe_water_source", "name_of_water_source", .), everything()) %>% 
    rename_with(~gsub("name_of_villages_cattle_camp", "reporting_unit", .), everything()) %>% 
    rename_with(~gsub("all_communities_using_water_source", "reporting_unit", .), everything()) %>% 
    rename_with(~gsub("village_id", "reporting_unit_code", .), everything()) %>% 
    rename_with(~gsub("endemic_villages_using_water_source_if_no_evs_then_1_villages_using", "ev_using_water_source", .), everything()) %>% 
    rename_with(~gsub("endemic_communities_using_water_sources", "ev_using_water_source", .), everything()) %>% 
    select(year,
           state,
           county,
           boma,
           payam,
           contains("supervisory_area"),
           contains("reporting_unit"),
           #contains("reporting_unit_code"),
           contains("abate_added"),
           contains("latitude"),
           contains("longitude"),
           name_of_water_source,
           contains("type_of_water_source"),
           ev_using_water_source,
           contains("combined_merged_water_source_name"),
           -contains("total")) 
    
  return(df)
}

# Stitch
df_abate_hist <- purrr::map_dfr(.x = files,
                           .f = ~ stitch_abate(.x))

# Clean and reshape df
df_abate_hist_clean <- df_abate_hist %>% 
  mutate(across(contains("volume"), as.numeric, na.rm = TRUE)) %>% 
  rowwise() %>% 
  mutate(volume_of_abate_added_ml_5 = sum(volume_of_abate_added_ml_5, volume_of_abate_added_ml_5_part_2, na.rm = TRUE),
         volume_of_abate_added_ml_6 = sum(volume_of_abate_added_ml_6, volume_of_abate_added_ml_6_part_2, na.rm = TRUE),
         volume_of_abate_added_ml_7 = sum(volume_of_abate_added_ml_7, volume_of_abate_added_ml_7_part_2, na.rm = TRUE),
         volume_of_abate_added_ml_9 = sum(volume_of_abate_added_ml_9, volume_of_abate_added_ml_9_part_2, na.rm = TRUE),
         volume_of_abate_added_ml_10 = sum(volume_of_abate_added_ml_10, volume_of_abate_added_ml_10_part_2, na.rm = TRUE),
         volume_of_abate_added_ml_12 = sum(volume_of_abate_added_ml_12, volume_of_abate_added_ml_12_part_2, na.rm = TRUE),
         across(c("state", "county", "payam", "boma", "supervisory_area", "reporting_unit", "name_of_water_source", 
                  "type_of_water_source", "ev_using_water_source"), str_to_title),
         cc = case_when(
           str_detect(reporting_unit, "CC") ~ "Cattle Camp",
           str_detect(reporting_unit, "Cc") ~ "Cattle Camp",
           is.na(reporting_unit) ~ NA,
           TRUE ~ "Village"),
         reporting_unit=str_replace(reporting_unit, "Cc", "CC")) %>% 
  select(-contains("part_2")) %>% 
  filter(!is.na(state) | !is.na(county) | !is.na(name_of_water_source)) %>% 
  left_join(df_geo_map_state, by = c("state" = "state_join")) %>% 
  left_join(df_geo_map_county, by = c("county" = "county_join")) %>% 
  mutate(
    state = case_when(
      is.na(state_fixed) ~ state,
      TRUE ~ state_fixed),
    county = case_when(
      is.na(county_fixed) ~ county,
      TRUE ~ county_fixed)) %>% 
  select(-contains(c("fixed", "join"))) %>%
  mutate(
    state = case_when(
      county == "Torit" ~ "Eastern Equatoria",
      county == "Jur River" ~ "Western Bahr El Ghazal",
      county == "Gogrial West" ~ "Warrap",
      county == "Rumbek Centre" ~ "Lakes",
      county == "Tonj South" ~ "Warrap",
      county == "Tonj East" ~ "Warrap",
      county == "Aweil Centre" ~ "Northern Bahr El Ghazal",
      county == "Lopa/Lafon" ~ "Eastern Equatoria",
      county == "Ikotos" ~ "Eastern Equatoria",
      TRUE ~ state)
    # county = case_when(
    #   state == "Lakes")
    ) %>% 
  pivot_longer(c(contains("volume")), names_to = "indicator", values_to = "value") %>%
  filter(value != 0 & !is.na(value)) %>% 
  mutate(month = as.numeric(str_extract(indicator, "\\d+"))) %>% 
  mutate(indicator = "abate_used",
         year = as.integer(year),
         month=month.name[as.numeric(month)],
         source = "Abate",
         sheet = "Abate")

# Create abate_treated variable and bind
df_abate_treated <- df_abate_hist_clean %>% 
  mutate(indicator = "abate_treated",
         value = 1)

df_abate_hist_clean_treated <- df_abate_hist_clean %>% 
  bind_rows(df_abate_treated)

# Read in historical data and bind
df_abate_hist_final <- read.csv("~/Github/dewormr/Dataout/gwsd_21_23.txt") %>% 
  filter(year < 2023,
         sheet == "Abate") %>% 
  select(-vas, -water_source_id) %>% 
  bind_rows(df_abate_hist_clean_treated) %>% 
  select(year, month, state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code, cc, type_of_water_source, 
         ev_using_water_source, combined_merged_water_source_name, name_of_water_source, indicator, value, reason_no_abate, risk_level, 
         latitude, longitude, value, source, sheet) %>% 
  write_xlsx(file.path(data_out, "south_sudan_abate_11_22.xlsx"))




