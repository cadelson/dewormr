# PURPOSE: Scratch
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: February 2, 2022
# NOTES:
data_out <- "~/Github/dewormr/Dataout"
current_month <- "October"

# Caclulate villages receiving abate by month for Global Review handout - February 14 '23
df_21_22 %>% 
  filter(indicator %in% c("abate_targeted", "abate_treated"),
         year == "2022",
         value > 0,
         month == "April"
         ) %>%
  group_by(month, indicator, county) %>% 
  distinct(reporting_unit, county) %>% 
  count() %>% 
  pivot_wider(names_from = indicator, values_from = n) %>% 
  mutate(ptreated = abate_treated/abate_targeted) %>% 
  View()
  


df %>% 
  pivot_longer(cols=`Jan-Apr`:`Dec`, names_to = "indicator", values_to = "value") %>% 
  write_xlsx(file.path(data_out, "cases.xlsx"))
  View() 

#Filter use 2021-2022 Curtis request
df_21_22 %>% 
  filter(indicator %in% c("filter_dist_pipe", "filter_dist_cloth")) %>% 
  group_by(year, month, county, boma, reporting_unit, indicator) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(total_filters = filter_dist_pipe + filter_dist_cloth) %>% 
  write_xlsx(file.path(data_out, "filters_distributed.xlsx"))

#Animal rumours for ARM footnotes
df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet %in% c("animals")) %>% 
  group_by(county, year, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(rumours_invest_24 = rumours_invest_24/rumours_total) %>% 
  View()
  

df_21_22 %>% 
  filter(indicator %in% c("report_expected", "hp_working", "hp_not_working"),
         risk_level == "Risk Level 3",
         vas == 1,
         year == "2022",
         month == "October") %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  filter(report_expected > 0) %>% 
  write_xlsx(file.path(data_out, "level_3_wash.xlsx"))
  #distinct(hp_not_working)
  count()
  summarise(across(where(is.numeric), sum, na.rm=TRUE))

# Samat filter distribution request
df_21_22 %>% 
  filter(indicator %in% c("filter_dist_cloth","filter_dist_pipe"),
         year == "2022") %>% 
  group_by(month, county, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  write_xlsx(file.path(data_out, "ssgwep_filter_distribution_2022.xlsx"))
  View()


#GPS request Yujing
df_21_22 %>% 
  filter(indicator == "report_expected",
         value>0,
         vas == 1,
         !is.na(latitude)) %>% 
  distinct(reporting_unit, boma, payam, county, state, latitude, longitude, year) %>% 
  write_xlsx(file.path(data_out, "ssgwep_gps_21_22.xlsx"))
  View()


#Yujing data request

df_21_22 %>% 
  filter(indicator %in% c("rumours_invest_24"),
         sheet %in% c("animals"),
         year == 2022) %>% 
  group_by(year, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  View()
  write_xlsx(file.path(data_out, "ssgwep_animal_rumours_21_22.xlsx"))

# Makoy
df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")) %>% 
  group_by(year, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df_21_22 %>% 
  filter(indicator %in% c("cr_reached")) %>% 
  group_by(year, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

###### Key indicators for risk level 1, ARM graphic

#Drinking water access
df_21_22 %>% 
  filter(year == "2022",
         vas == 1,
         month == current_month,
         indicator %in% c("report_received", "hp_working")) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  filter(report_received == 1) %>%
  mutate(
    "hp_working" = case_when(
      hp_working > 0 ~ 1,
      TRUE ~0),
    "drinking access" = sum(hp_working)/sum(report_received)) %>% 
  #View()
  summarise(across(c(report_received, hp_working), sum, na.rm = TRUE)) %>% 
  View()

#VV Coverage
  df_21_22 %>% 
    filter(year == "2022",
           vas == 1,
           month == current_month,
           indicator %in% c("report_received", "staff_vv", "hh_total")) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    filter(report_received == 1,
           hh_total > 0) %>%
    mutate(
      "staff_vv" = case_when(
        staff_vv > 0 ~ 1,
        TRUE ~0),
      "vv_coverage" = sum(staff_vv)/sum(report_received)) %>% 
    #View() %>% 
    summarise(across(c(report_received, staff_vv), sum, na.rm = TRUE)) %>% 
    View()
  
  
  #Monthly Health Education
  df_21_22 %>% 
    filter(year == "2022",
           vas == 1,
           month == current_month,
           indicator %in% c("report_received", "activity_cra", "hh_total")) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    filter(report_received == 1,
           hh_total > 0) %>%
    mutate(
      "activity_cra" = case_when(
        activity_cra > 0 ~ 1,
        TRUE ~0),
      "cra_coverage" = sum(activity_cra)/sum(report_received)) %>% 
    #View() %>% 
    summarise(across(c(report_received, activity_cra), sum, na.rm = TRUE)) %>% 
    View()
  
  #Submitted reports
  df_21_22 %>% 
    filter(year == "2022",
           vas == 1,
           #month == current_month,
           indicator %in% c("report_received", "report_expected")) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    filter(report_expected > 0 ) %>%
    mutate(
      "report_expected" = case_when(
        report_expected > 0 ~ 1,
        TRUE ~0),
      "report_coverage" = sum(report_received)/sum(report_expected)) %>% 
    #View()
    summarise(across(c(report_received, report_expected), sum, na.rm = TRUE)) %>% 
    View()
  
  
  #Risk level one filter coverage
  df_21_22 %>% 
    filter(indicator %in% c("filter_hh_cloth", "hh_total", "report_expected", "report_received"),
           month == current_month,
           year=="2022",
           risk_level == "Risk Level 1",
           vas=="1") %>% 
    pivot_wider(names_from=indicator) %>% 
    filter(report_expected== 1,
           report_received== 1,
           hh_total > 0) %>% 
    mutate(filter_coverage=filter_hh_cloth/hh_total,
           coverage_tally=case_when(
             filter_coverage>.95 ~ 1,
             filter_coverage<.95 ~ 0),
           total=1) %>% 
    group_by(year, month, vas) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    mutate(coverage=coverage_tally/total) %>% 
    #arrange(match(month, c("January", "February", "March", "April", "May", "June"))) %>% 
    select(year, month, coverage_tally, total, coverage) %>% 
    View()
  
  #Abate Treatment
  df_21_22 %>% 
    filter(year == "2022",
           vas == 1,
           risk_level == "Risk Level 1",
           month == current_month,
           indicator %in% c("report_received", "abate_ws_treated", "hh_total")) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    filter(report_received == 1,
           hh_total > 0) %>%
    mutate(
      "abate_ws_treated" = case_when(
        abate_ws_treated > 0 ~ 1,
        TRUE ~0),
      "abate_coverage" = sum(abate_ws_treated)/sum(report_received)) %>% 
    summarise(across(c(report_received, abate_ws_treated), sum, na.rm = TRUE)) %>% 
    View()
  
    #Geographic Coordinates
    df_21_22 %>% 
      filter(year == "2022",
             vas == 1,
             month == current_month,
             indicator %in% c("report_received")) %>% 
      pivot_wider(names_from = indicator, values_from = value) %>% 
      filter(report_received > 0) %>% 
      mutate(
        "latitude_present" = case_when(
          !is.na(latitude) ~ 1,
          TRUE ~0),
        "gps_coverage" = sum(latitude_present)/sum(report_received)) %>% 
      #group_by(county) %>% 
      summarise(across(c(report_received, latitude_present), sum, na.rm = TRUE)) %>%
      mutate(percent = percent(latitude_present/report_received)) %>% 
      rename("Have GPS Data" = latitude_present,
             "Total VAS" = report_received) %>% 
      View()
    
# Filter missing GPS for SPOs
    df_21_22 %>% 
      filter(year == "2022",
             vas == 1,
             month == current_month,
             indicator %in% c("report_expected")) %>% 
      pivot_wider(names_from = indicator, values_from = value) %>% 
      filter(report_expected > 0,
             is.na(latitude)) %>% 
      write_xlsx(file.path(data_out, "VAS With Missing GPS September 2022.xlsx"))
      View()
      
    
# WASH information for Jim Nov 3, 2022
df_21_22 %>% 
  filter(
    year == "2022",
    vas == 1,
    risk_level == "Risk Level 2",
    month == "October",
    indicator %in% c("hp_working", "hp_not_working", "report_received")) %>% 
    #county %in% c("Tonj East", "Awerial", "Rumbek North", "Uror", "Lopa/Lafon")) %>%
  pivot_wider(names_from = indicator, values_from = value) %>% 
  filter(report_received == "1") %>% 
  select(state, county, payam, supervisory_area, reporting_unit, latitude, longitude, cc, hp_working, hp_not_working) %>% 
  mutate("Working Borehole" = case_when(hp_working >0 ~ "Has Working BH", TRUE ~ "No Working BH"),
         "Broken Borehole" = case_when(hp_not_working >0 ~ "Has Broken BH", TRUE ~ "No Broken BH")) %>% 
  #filter(`Working Borehole` == "No Working BH") %>% 
  View()
  write_xlsx(file.path(data_out, "SSGWEP WASH Indicators September 2022 v2.xlsx"))
  View()
  


#VAS populations for Jarweng
df_21_22 %>% 
  filter(year == "2022",
         month =="July",
         boma == "Jarweng",
         indicator == "hh_total") %>% 
  select(county, payam, boma, supervisory_area, reporting_unit, indicator, year, month, value) %>% 
  View()

# Lopa/Lafon Rumours in 2021/2022
df_21_22 %>% 
  filter(
    county == "Lopa/Lafon",
    indicator %in% c("rumours_total", "suspects_total"),
    sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")
  ) %>% 
  group_by(year, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

#Stephen request for VAS, HH, CCs
df_21_22 %>% 
  filter(month == "July",
         year == "2022",
         vas == 1,
         indicator %in% c("hh_total", "pop_total", "report_expected")) %>% 
  select(state, county, payam, boma, supervisory_area, reporting_unit, cc, indicator, value) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  filter(report_expected>0) %>% 
  write_xlsx(file.path(data_out, "vas_for_stephen_data.xlsx"))


#Yujing data ask for CDC
df_21_22 %>% 
  filter(year == "2021",
         vas == 1,
         month == "December",
         sheet == "MSR_Surv",
         indicator == "report_expected",
         value == 1) %>% 
  distinct(vas, state, county, payam, boma, reporting_unit) %>% 
  summarise(total=sum(vas))

df_21_22 %>% 
  filter(year == "2021",
         vas == 1,
         month == "December",
         sheet == "MSR_Surv",
         indicator == "report_received",
         value == 1) %>% 
  distinct(vas, state, county, payam, boma, reporting_unit) %>% 
  summarise(total=sum(vas))


#Uror filter coverage for provisional analysis
df_21_22 %>% 
  filter(indicator %in% c("filter_hh_cloth", "hh_total", "report_expected", "report_received"),
         month %in% c("January", "February", "March", "April", "May", "June", "July"),
         year=="2022",
         county=="Uror",
         vas=="1",
         sheet=="MSR_Surv") %>% 
  pivot_wider(names_from=indicator) %>% 
  filter(report_expected=="1",
         report_received=="1",
         hh_total!="0") %>% 
  mutate(filter_coverage=filter_hh_cloth/hh_total,
         coverage_tally=case_when(
           filter_coverage>.95 ~ 1,
           filter_coverage<.95 ~ 0),
         total=1) %>% 
  group_by(year, month, county, vas) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  mutate(coverage=coverage_tally/total) %>% 
  arrange(match(month, c("January", "February", "March", "April", "May", "June"))) %>% 
  select(year, month, coverage_tally, total, coverage) %>% 
  View()


df_21_22_path <- "~/Github/dewormr/Dataout/gwsd_21_22.txt"
df_21_22<- read.csv(df_21_22_path)
df_21_22 %>% View()


df_21_22 %>% 
  filter(year=="2022",
         month=="March",
         indicator %in% c("staff_vv", "hh_total")) %>%
  View()
  select(county, payam, reporting_unit, indicator, month, value) %>% 
  group_by(county, payam, indicator) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  View()
df_21_22 %>% glimpse()
