# PURPOSE: Munge and analyze 2024 MSR-Surv, Cash Reward and Abate Databases for routine analyses and append with historicaldata
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: March 28, 2024
# NOTES: August 2022 - Functionalized script, updated read in of files using purrr package, long mutate of indicator and risk levels changed to a join
# Can remove abate reason untreated indicator

get_gwsd("November")
get_gwsd <- function(current_month) {
  
  library(tidyverse)
  library(glue)
  library(readxl)
  library(janitor)
  library(purrr)
  library(sf)
  
  #============== Set file paths and import data
  
  data_in <-glue("~/Github/dewormr/Data/{match(current_month, month.name)}. Databases ({current_month} 2024)")
  
  #data_in <-"~/Github/dewormr/Data/11. Databases (November 2024)"
  
  data_out <- "~/Github/dewormr/Dataout"
  ind_conv <- read_xlsx("~/Github/dewormr/Data/indicator_conversion.xlsx") %>% mutate(indicator = trimws(indicator)) %>% distinct()
  risk_levels <- read_xlsx("~/Github/dewormr/Data/risk_levels.xlsx", col_types = "text")
  `%notin%` <- Negate(`%in%`)
  
  (df_imports <- list.files(data_in, full.names = TRUE) %>% 
      purrr::map_dfr(~readxl::excel_sheets(.) %>% 
                       tibble::as_tibble_col(., column_name = "sheet") %>% 
                       dplyr::mutate(file = .x, .before = "sheet")) %>% 
      filter(str_detect(sheet, "msr|abate|animal|rumours|idsr")))
  
  df_imports %>%
    purrr::pmap(function(file, sheet){
      df <- readxl::read_excel(file, sheet)
      assign(glue::glue("df_{sheet}"), df, envir=globalenv())})
  
  #============== Munge each df to get ready for merge
  #MSR Surv Data
  df_msr_surv <- df_msr_surv %>% 
    select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER/SFC`:-`NAME OF CHIEF`) %>% 
    pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>% 
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="MSR_Surv",
           LATITUDE = as.character(LATITUDE),
           LONGITUDE = as.character(LONGITUDE))
  
  #Non MSR
  df_non_msr_surv <- df_non_msr_surv %>% 
    select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER/SFC`:-`NAME OF CHIEF`) %>% 
    pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>% 
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="Non_MSR_Surv",
           LATITUDE = as.character(LATITUDE),
           LONGITUDE = as.character(LONGITUDE))
  
  #Non MSR Surv - Other
  df_non_msr_surv_other <- df_non_msr_surv_other %>% 
    pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>% 
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="Non_MSR_Other")
  
  # MSR IDSR
  df_idsr <- df_idsr %>%
    pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>%
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="IDSR")
  
  # Hotline Report
  df_hotline_rumours <- df_hotline_rumours %>%
    rename_all(toupper) %>%
    pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>% 
    separate(indicator, c("indicator", "month"), sep="-") %>%
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="hotline")
  
  # Animal Rumours
  df_animal_rumours <- df_animal_rumours %>%
    #select(-(starts_with("Animal Types"))) %>%
    mutate(cc_animals=as.character(cc_animals)) %>% 
    pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>%
    mutate(month=month.name[as.numeric(month)],
           source="MSR_Surv",
           sheet="animals")
  
  # CR - Surv
  df_msr_cr <- df_msr_cr %>% 
    select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`, -starts_with("...")) %>% 
    pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>% 
    mutate(month=month.name[as.numeric(month)],
           source="MSR_CR",
           sheet="MSR_CR",
           LATITUDE = as.character(LATITUDE),
           LONGITUDE = as.character(LONGITUDE))
  
  # CR - Non MSR
  df_non_msr_cr <- df_non_msr_cr %>% 
    select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`) %>%
    pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
    separate(indicator, c("indicator", "month"), sep="-") %>% 
    mutate(month=month.name[as.numeric(month)],
           LATITUDE = as.character(LATITUDE),
           LONGITUDE = as.character(LONGITUDE),
           source="MSR_CR",
           sheet="Non_MSR_CR")
  
  # Abate
  df_abate <- df_abate %>%
    filter(STATE!="NA") %>%
    select(STATE:`Type of Water Source`, Latitude:`Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using`,
           contains("Targeted"), contains("Eligible"), contains("Treated"), contains("Amount")) %>% 
    mutate(across(c(contains("Y = 1"), contains("Amount")), as.character)) %>%
    rename("LATITUDE"=Latitude,
           "LONGITUDE"=Longitude) %>% 
    pivot_longer(c(contains("Y = 1"), contains("If not treated, why?"), contains("Amount")), names_to="indicator", values_to="value") %>%
    mutate(indicator=gsub("\\(P.*", "", indicator)) %>%
    separate(indicator, c("indicator", "month"), sep="-") %>%
    filter(!is.na(value)) %>% 
    mutate(reason_no_abate=value,
           reason_no_abate=replace(reason_no_abate, reason_no_abate %notin% c("DRY", "NEGATIVE CDC RESULT", 
                                                                              "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA),
           value=as.numeric(replace(value, value %in% c("DRY", "NEGATIVE CDC RESULT", 
                                                        "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA)),
           month=month.name[as.numeric(month)],
           LATITUDE = as.character(LATITUDE),
           LONGITUDE = as.character(LONGITUDE)
    ) %>% 
    arrange(STATE, COUNTY, PAYAM, BOMA, `SUPERVISORY AREA`, `REPORTING UNIT`, `Name of Water Source`, month, reason_no_abate) %>% 
    group_by(STATE, COUNTY, PAYAM, BOMA, `SUPERVISORY AREA`, `REPORTING UNIT`, `Name of Water Source`, month) %>% 
    mutate(indicator=fct_relabel(indicator, trimws)) %>% 
    fill(reason_no_abate, .direction = "down") %>% 
    group_by(STATE, COUNTY, PAYAM, BOMA, `SUPERVISORY AREA`, `REPORTING UNIT`, `REPORTING UNIT CODE`, `Name of Water Source`, `Combined/ Merged Water Source Name`, `Water Source ID`, `Type of Water Source`, LATITUDE, LONGITUDE, `Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using`, month, reason_no_abate, indicator) %>% 
    summarise(value = sum(value)) %>% 
    ungroup() %>% 
    mutate(indicator = case_when(
      indicator == "Targeted\r\n(Y = 1, N = 0)" ~ "abate_targeted",
      indicator == "Eligible\r\n(Y = 1, N = 0)" ~ "abate_eligible",
      indicator == "Treated\r\n(Y = 1, N = 0)" ~ "abate_treated",
      indicator == "Amount of Abate Added (mL)" ~ "abate_used",
      indicator == "If not treated, why?" ~ "abate_reason_untreated"),
      value = case_when(
        indicator == "abate_reason_untreated" ~ 0,
        TRUE ~ value),
      source="Abate",
      sheet="Abate") 
  
  #============== Combine databases into one df and clean
  
  df_rough <- bind_rows(df_msr_surv, df_non_msr_surv, df_non_msr_surv_other, df_idsr, df_hotline_rumours,
                        df_animal_rumours, df_msr_cr, df_non_msr_cr, df_abate)
  
  df <- df_rough %>% 
    clean_names() %>% remove_empty() %>%
    mutate_all(trimws, which="both") %>% 
    mutate(month_num = as.integer(factor(month, levels = month.name)),
           month_cur = as.integer(factor(current_month, levels=month.name)),
           #month_cur = as.integer(factor("January", levels=month.name)),
           across(c("state", "county", "payam", "boma", "supervisory_area",
                    "reporting_unit", "name_of_water_source", 
                    "type_of_water_source",
                    "endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using", "reason_no_abate"), str_to_title)) %>%
    filter(month_cur>=month_num) %>% 
    select(-month_num, -month_cur) %>% 
    rename(vas=village_under_active_surveillance_1_yes_0_no,
           ev_using_water_source=endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using) %>% 
    mutate(
      cc = case_when(
        cc_animals == "1" ~ "1",
        str_detect(reporting_unit, "CC") ~ "1",
        str_detect(reporting_unit, "Cc") ~ "1",
        TRUE ~ "0"),
      payam=case_when(
        payam=="Pulchol" ~"Pulchuol",
        payam=="Nile" & reporting_unit=="Panakech"~"Dor",
        TRUE~payam),
      reporting_unit=case_when(
        reporting_unit=="Wechkotda"|reporting_unit=="Wechkoda"|reporting_unit=="Wech Kotda"|reporting_unit=="Wech Koteda"|reporting_unit=="Wechkoida" ~ "Wechotda",
        reporting_unit %in% c("Nyakhar Manyak") ~ "Nyakhor Manyak",
        reporting_unit=="Wun Thony" ~ "Wunethony",
        reporting_unit=="Nyakhor  Kamel" ~ "Nyakhor Kamel",
        TRUE~reporting_unit),
      reporting_unit=str_replace(reporting_unit, "Cc", "CC"),
      indicator=tolower(indicator),
      latitude = as.character(latitude),
      longitude = as.character(longitude),
      host = case_when(
        is.na(host) & indicator %in% c("rumours_self", "rumours_informer", "rumours_total", "rumours_invest", "rumours_invest_24",
                                       "suspects_total", "suspects_healed") ~ "Human",
        TRUE ~ host
      )) %>% 
    left_join(ind_conv) %>% 
    select(-indicator, -cc_animals) %>% 
    rename("indicator" = indicator_new) 
  
  
  
  #============== Remove dfs, combine with historical data, more cleaning and write to csv
  
  df_21_23_path <- "~/Github/dewormr/Dataout/gwsd_21_23.txt"
  df_21_23<- read.csv(df_21_23_path) %>% 
    mutate(year = as.character(year)) %>% 
    filter(!is.na(indicator))
  
  df_21_24 <- df %>% 
    mutate(year="2024",
           vas=as.integer(vas),
           value=as.numeric(value)) %>%
    left_join(risk_levels) %>% 
    mutate(risk_level = replace_na(risk_level, "Risk Level 3")) %>% 
    bind_rows(df_21_23) %>% 
    mutate(
      county = case_when(
        county == "Lopa/Lafon" ~ "Lafon",
        TRUE ~ county),
      state = case_when(
        state %in% c("Ces", "Central Euaatoria") ~ "Central Equatoria",
        TRUE ~ state),
      "cc"=case_when(
        str_detect(cc, "1") ~ "Cattle Camp",
        TRUE ~ "Village"),
      "value" = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    select(-contains("x11"), -combined_merged_water_source_name, -water_source_id)
  
  df_21_24 %>% 
    write_csv(file.path(data_out, "gwsd_21_24.txt"))
  
  df_21_24 %>%
    filter(year %in% c("2022", "2023", "2024")) %>% 
    write_csv(file.path(data_out, "gwsd_22_24.txt"))
  
  
  
  rm(df_msr_cr, df_msr_surv, df_abate, df_idsr, df_animal_rumours, df_hotline_rumours, df_non_msr_surv_other, df_non_msr_cr,
     df_non_msr_surv, df_imports)
  return(df_21_24)
  
  rm(list = ls())
}


