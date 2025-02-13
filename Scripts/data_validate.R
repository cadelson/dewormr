# PURPOSE: Validate current year GW data
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: Nov 14, 2024
# NOTES: 

library(tidyverse)
library(writexl)
library(sf)
library(janitor)

data_out <- "~/Github/dewormr/Dataout"

df_22_24 <- read.csv("~/Github/dewormr/Dataout/gwsd_22_24.txt")

ss_county <- st_read("~/Github/gis/Data/gis_data/ssd_admbnda_adm2_imwg_nbs_20180401.shp")

`%notin%` <- Negate(`%in%`)

current_year <- df_22_24 %>% 
  summarise(max_year = max(year)) %>% 
  pull(max_year)

current_month <- df_22_24 %>% 
  filter(year == current_year) %>% 
  mutate(month = match(month, month.name)) %>% 
  summarise(max_month = max(month)) %>% 
  mutate(max_month = month.name[as.numeric(max_month)]) %>%
  pull(max_month)

# Missing reports
df_missing <- df_22_24 %>% 
  filter(year == current_year,
         indicator %in% c("report_expected", "report_received")) %>% 
  select(vas, state, county, payam, boma, supervisory_area, reporting_unit, 
         reporting_unit_code, indicator, month, value) %>%
  pivot_wider(names_from = c(indicator), values_from=value) %>% 
  mutate(report_received = replace_na(report_received, 0)) %>% 
  filter(report_expected == 1 & report_received == 0) %>% 
  mutate(issue = "report missing")

# Duplicate reports
df_duplicate <- df_22_24 %>% 
  filter(year == current_year,
         indicator == "report_expected" & value == 1 | indicator == "abate_targeted" & value == 1) %>% 
  group_by(sheet, state, county, payam, boma, reporting_unit, month, name_of_water_source) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  distinct(sheet, state, county, payam, boma, reporting_unit, name_of_water_source) %>% 
  arrange(sheet, state, county, payam, boma)

# Empty rows with values
df_empty <- df_22_24 %>% 
  filter(year == current_year,
         is.na(state) | is.na(county),
         value > 0) %>% 
  remove_empty()

# Rumours
df_rumours_issues <- df_22_24 %>% 
  filter(year == current_year,
         indicator %in% c("rumours_total", "suspects_total", "rumours_invest_24", "rumours_invest")) %>% 
  select(vas, state, county, payam, boma, supervisory_area, reporting_unit, 
         reporting_unit_code, indicator, month, value, sheet, cc, host) %>%
  pivot_wider(names_from = c(indicator), values_from=value) %>% 
  rowwise() %>% 
  mutate(percent_invest = rumours_invest / rumours_total,
         percent_invest_24 = rumours_invest_24 / rumours_total,
         percent_suspects = suspects_total / rumours_total) %>% 
  ungroup() %>% 
  mutate(issue = case_when(
    suspects_total > rumours_total ~ "Suspects greater than total rumours",
    suspects_total > rumours_invest ~ "Suspects greater than rumours investigated",
    suspects_total > rumours_invest_24 ~ "Suspects greater than rumours investigated in 24 hours",
    rumours_invest > rumours_total ~ "Investigated rumours greater than total rumours",
    rumours_invest_24 > rumours_total ~ "Rumours investigated in 24 hours greater than total rumours",
    percent_invest < 1 ~ "Not all rumours investigated",
    percent_invest_24 < .8 & rumours_total > 8 ~ "Low investigation in 24 hours, please check to confirm")) %>% 
  select(-contains("percent")) %>% 
  filter(!is.na(issue))


# Abate issues
df_abate_issue <- df_22_24 %>%
  filter(indicator %in% c("abate_targeted", "abate_eligible", "abate_treated"),
         year == current_year) %>% 
  select(state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code, month, 
         value, name_of_water_source, type_of_water_source, ev_using_water_source, reason_no_abate, indicator,
         latitude, longitude, year) %>% 
  # Count repetitions for each water source per indicator
  group_by(reporting_unit, name_of_water_source, indicator, month) %>%
  mutate(repeat_count = row_number()) %>%
  ungroup() %>%
  # Modify water source name if repeat_count exceeds 1
  mutate(name_of_water_source = ifelse(repeat_count > 1, 
                                       paste(name_of_water_source, "repeat number n =", repeat_count), 
                                       name_of_water_source)) %>%
  select(-repeat_count) %>%
  pivot_wider(names_from=indicator, values_from=value) %>% 
  #select(-row) %>% 
  #filter(abate_targeted<abate_eligible | abate_eligible<abate_treated | abate_targeted<abate_treated | abate_targeted==1 & abate_eligible==1 & abate_treated==0 & is.na(reason_no_abate)) %>% 
  mutate(issue = case_when(
    abate_eligible > abate_targeted ~ "Water source eligible but not targeted",
    abate_treated > abate_eligible ~ "Water source treated but not eligible",
    abate_treated == 1 & !is.na(reason_no_abate) ~ "Water source treated but given reason for no abate",
    abate_targeted > 0 & abate_eligible == 0 & abate_treated == 0 & is.na(reason_no_abate) ~ "Water source not treated but no reason given for no abate",
    abate_eligible == 1 & !is.na(reason_no_abate) ~ "Water source eligible but reason given for no abate",
    abate_eligible > 0 & abate_treated == 0 ~ "Water source eligible but not treated")) %>% 
  select(year, state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code, 
         name_of_water_source, type_of_water_source, ev_using_water_source, month, abate_targeted, abate_eligible, abate_treated,
         reason_no_abate, issue) %>% 
  filter(!is.na(issue),
         abate_targeted < 2)

df_abate_used <- df_22_24 %>% 
  filter(year == current_year,
         indicator == "abate_used") %>% 
  select(year, state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code, month, 
         name_of_water_source, type_of_water_source, ev_using_water_source, indicator, value) %>%
  filter(value > 40000) %>% 
  mutate(issue = "Abate use seems high, please confirm")

df_missing_gps <- df_22_24 %>% 
  filter(year == current_year) %>% 
  distinct(state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code,
           latitude, longitude, name_of_water_source, sheet) %>% 
  filter(is.na(latitude) | is.na(longitude),
         sheet %in% c("MSR_Surv", "Abate", "Non_MSR_Surv")) %>% 
  mutate(issue = "Missing latitude or longitude")

df_validate_gps <- df_22_24 %>% 
  distinct(year, state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code,
           latitude, longitude, name_of_water_source, sheet) %>% 
  filter(year == current_year,
         !is.na(latitude),
         !is.na(longitude),
         sheet %in% c("MSR_Surv", "Abate", "Non_MSR_Surv")) %>% 
  mutate(across(c(latitude, longitude),
                ~ ifelse(grepl("\\°", .), gsub(",", ".", .), .)),
         longitude_parse = suppressWarnings(parzer::parse_lon(longitude)),
         latitude_parse= suppressWarnings(parzer::parse_lat(latitude)))
  
parse_failure <- df_validate_gps %>% 
  filter(longitude_parse == "NaN" | latitude_parse == "NaN") %>% 
  mutate(issue = case_when(
    longitude_parse == "NaN" & latitude_parse != "NaN" ~ "Longitude failed to parse",
    longitude_parse != "NaN" & latitude_parse == "NaN" ~ "Latitude failed to parse",
    longitude_parse == "NaN" & latitude_parse == "NaN" ~ "Latitude and Longitude failed to parse"))

ss_county_sf <- st_as_sf(ss_county, wkt = "geometry")

df_incorrect_location_conversion <- df_validate_gps %>%
  filter(longitude_parse != "NaN" & latitude_parse != "NaN") %>% 
  mutate(longitude_geography = longitude_parse,
         latitude_geography = latitude_parse) %>% 
  st_as_sf(coords = c("longitude_geography", "latitude_geography"), crs = 4326) %>%
  st_transform(crs = st_crs(ss_county_sf))

calculate_min_distance <- function(gps_point, county_geom) {
  distances <- st_distance(gps_point, county_geom, by_element = TRUE)
  min(distances)
}

df_with_county <- st_join(df_incorrect_location_conversion, ss_county_sf, join = st_within, left = TRUE)

# Calculate minimum distance for each GPS point to its corresponding county
df_with_distances <- df_with_county %>%
  rowwise() %>%
  mutate(
    county_geom = list(st_geometry(ss_county_sf[ss_county_sf$admin2Name == county, ])),
    min_distance_km = as.numeric(calculate_min_distance(geometry, county_geom)) / 1000) %>%
  ungroup()

# Filter rows with distance > 25 km
df_incorrect_location <- df_with_distances %>%
  filter(min_distance_km > 25) %>%
  as_tibble() %>%
  select(-contains("admin"), -date, -validOn, -validTo, -geometry, -county_geom) %>% 
  mutate(issue = "GPS point located >25km outside of county") %>% 
  rename(distance_to_county = min_distance_km) %>% 
  bind_rows(parse_failure, df_missing_gps) %>% 
  select(sheet, state, county, payam, boma, supervisory_area, reporting_unit, reporting_unit_code,
         name_of_water_source, latitude, longitude, latitude_parse, longitude_parse, distance_to_county, issue) %>% 
  arrange(sheet, issue, state, county, payam, boma, supervisory_area, reporting_unit, name_of_water_source)

write_xlsx(list(
  "Missing Reports" = df_missing,
  "Duplicate Reports" = df_duplicate,
  "Values with No Geography" = df_empty,
  "Rumour Issues" = df_rumours_issues,
  "Logical Abate Issues" = df_abate_issue,
  "High Abate Use" = df_abate_used, 
  "GPS Issues" = df_incorrect_location),
  path = file.path(data_out, paste0("gw_data_validation_issues_", tolower(current_month), "_", current_year, ".xlsx")))

    
    