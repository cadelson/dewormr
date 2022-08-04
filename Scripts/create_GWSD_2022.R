# PURPOSE: Munge and analyze 2022 MSR-Surv, Cash Reward and Abate Databases for routine analyses and append with 2021 data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: March 8, 2022
# NOTES: August 2022 - Functionalized script, updated read in of files using purrr package, long mutate of indicator and risk levels changed to a join
# TO DO: 
# Add validation rules to raw data from census for payam/boma
# Remove color from raw data, add notes col
# Clean raw abate data and condense code

get_gwsd("June")
get_gwsd <- function(current_month) {
  
#============== Set file paths and import data
data_in <-glue("~/Github/dewormr/Data/{match(current_month, month.name)}. Databases ({current_month} 2022)")
data_out <- "~/Github/dewormr/Dataout"
ind_conv <- read_xlsx("~/Github/dewormr/Data/indicator_conversion.xlsx")
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
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`, -`Number of VVs - 2021`:-`Received - 2021`) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="MSR_Surv")

#Non MSR
df_non_msr_surv <- df_non_msr_surv %>% 
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`, -`Number of VVs - 2021`:-`Received - 2021`) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="Non_MSR_Surv")

#Non MSR Surv - Other
df_non_msr_surv_other <- df_non_msr_surv_other %>% 
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="Non_MSR_Other")

# MSR IDSR
df_idsr <- df_idsr %>%
  select(-`NO.RUMOURS-Totals`:-`SUSPECTS - Totals`) %>%
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
  select(-(starts_with("Animal Types"))) %>%
  mutate(cc=as.character(cc)) %>% 
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="animals")

# CR - Surv
df_msr_cr <- df_msr_cr %>% 
  select(-`NUMBER+I:I OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`, -starts_with("...")) %>% 
  pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_CR",
         sheet="MSR_CR")

# CR - Non MSR
df_non_msr_cr <- df_non_msr_cr %>% 
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)`, -`SENIOR PROGRAM OFFICER`:-`NAME OF CHIEF`, -`Total Number of Residents Reached with Cash Reward Messages - 2022`: -`SSGWEP Secretariat - 2022`) %>%
  pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
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
  mutate(reason_no_abate=value,
         reason_no_abate=replace(reason_no_abate, reason_no_abate %notin% c("DRY", "NEGATIVE CDC RESULT", 
                                                                            "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA),
         value=as.numeric(replace(value, value %in% c("DRY", "NEGATIVE CDC RESULT", 
                                           "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA)),
         month=month.name[as.numeric(month)],
         source="Abate",
         sheet="Abate") %>% 
  mutate(row = row_number(),
         indicator=fct_relabel(indicator, trimws)) %>% 
  pivot_wider(names_from=indicator, values_from = value) %>% 
  select(-row) %>% 
  group_by(STATE, COUNTY, PAYAM, BOMA, `SUPERVISORY AREA`, `REPORTING UNIT`, `REPORTING UNIT CODE`,
           `NAME OF WATER SOURCE`, `Combined/ Merged Water Source Name`, `Water Source ID`, 
           `Type of Water Source`, LATITUDE, LONGITUDE, `Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using`,
           month, reason_no_abate, source, sheet) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    "abate_targeted"=`Targeted\r\n(Y = 1, N = 0)`,
    "abate_eligible"=`Eligible\r\n(Y = 1, N = 0)`,
    "abate_treated"=`Treated\r\n(Y = 1, N = 0)`,
    "abate_used"=`Amount of Abate Added (mL)`,
    "abate_eligible"=case_when(
      abate_treated=="1" ~ "1",
      abate_treated=="0" ~ "0",
      TRUE ~ as.character(abate_eligible)),
    "abate_targeted"=as.double(abate_targeted),
    "abate_eligible"=as.double(abate_eligible),
    "abate_treated"=as.double(abate_treated)) %>% 
  select(-`Targeted\r\n(Y = 1, N = 0)`, -`Eligible\r\n(Y = 1, N = 0)`, -`Treated\r\n(Y = 1, N = 0)`, -`Amount of Abate Added (mL)`) %>% 
  pivot_longer(c(contains("abate_"), contains("If not treated, why?")), names_to="indicator", values_to="value")

#============== Combine databases into one df and clean

df_rough <- bind_rows(df_msr_surv, df_non_msr_surv, df_non_msr_surv_other, df_idsr, df_hotline_rumours,
                    df_animal_rumours, df_msr_cr, df_non_msr_cr, df_abate)

df <- df_rough %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(month_num = as.integer(factor(month, levels = month.name)),
         month_cur = as.integer(factor(current_month, levels=month.name)),
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
      str_detect(reporting_unit, "CC") ~ 1,
      str_detect(reporting_unit, "Cc") ~ 1,
      TRUE ~ 0),
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
    indicator=tolower(indicator)) %>% 
  left_join(ind_conv) %>% 
  select(-indicator) %>% 
  rename("indicator" = indicator_new)

#============== Combine with 2021 data, more cleaning and write to csv

df_21_path <- "~/Github/dewormr/Dataout/GWSD_2021_Final.txt"
df_21<- read.csv(df_21_path)

df_21_22 <- df %>% 
  mutate("year"="2022",
         "vas"=as.integer(vas),
         "value"=as.numeric(value)) %>%
  bind_rows(df_21) %>% 
  select(-latitude_2, -longitude_2) %>% 
  left_join(risk_levels) %>% 
  mutate(
    risk_level = replace_na(risk_level, "Risk Level 3"),
    "year"= case_when(
      year=="2022" ~ "2022",
      TRUE ~ "2021"),
    "cc"=case_when(
      str_detect(cc, "1") ~ "Cattle Camp",
      TRUE ~ "Village")) %>% 
  write_csv(file.path(data_out, "gwsd_21_22.txt"))

rm(df_msr_cr, df_msr_surv, df_abate, df_idsr, df_animal_rumours, df_hotline_rumours, df_non_msr_surv_other, df_non_msr_cr,
   df_non_msr_surv, df_rough, ind_conv, risk_levels, df_imports, df, df_21)
}
