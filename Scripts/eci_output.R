# PURPOSE: Output a file to create pivot table for endemic cluster intervention analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 29, 2021
# NOTES: May 22 - Add Wunthou reporting unit for Awerial, ask Deng if includes CC


`%notin%` <- Negate(`%in%`)
data_out <- "~/Github/dewormr/Dataout"
current_ec<-c("Block 1", "Wunethony", "Nyakhor Kamel", "Nyakhor Manyak", "Wechotda", "Kengel CC",
              "Baragep", "Malueth CC", "Keny CC", "Bardhiak CC", "Wunbul CC", "Mayom CC", "Tomrok",
              "Panakech", "Wungai CC", "Ruop CC", "Manyiel CC", "Apukdit", "Ajakdit", "Ageer", "Rumdit", "Rumkier",
              "Rumkiir", "Gel-Angon CC Zone 2", "Malueth Bai V/CC")
past_ec<-c("Ameer", "Ngadiang CC", "Parieng CC", "Panhomchet CC Zone", "Achol Manak")


df_eci_output <- df_21_22 %>% 
  select(-vas, -name_of_water_source, -combined_merged_water_source_name, -water_source_id,
         -type_of_water_source, -ev_using_water_source, -reason_no_abate, -risk_level) %>% 
  filter(reporting_unit %in% c(current_ec, past_ec),
         supervisory_area %notin% c("Gaak", "Ugel"),
         county %in% c("Uror", "Rumbek North", "Awerial", "Tonj East", "Tonj South"),
         #March 31, 2022 - Filtering out Rumdit from Makuac area, not in endemic cluster
         payam %notin% c("Makuac", "Wuror", "Pathai"),
         indicator %in% c("cases_new", "staff_vv", "hp_working", "hh_total", "filter_hh_cloth",
                          "pop_total", "filter_dist_pipe", "abate_targeted", "abate_eligible",
                          "abate_treated", "activity_cra", "cr_days", "cr_reached", "ed_video",
                          "ed_audio", "ed_integrated", "visit_as_hw", "visit_pso_fo", "visit_cso_po_spo_ta", 
                          "report_expected", "report_received"),
         indicator %notin% c("visit_as_hw", "visit_pso_fo", "visit_cso_po_spo_ta") | source!="MSR_CR" ) %>%
  mutate(row=row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  select(-row) %>% 
  group_by(state, county, payam, reporting_unit, month, year, cc) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(
    "Current or Past 2021 Endemic Cluster"=case_when(
      reporting_unit %in% current_ec ~ "Current 2021 Endemic Cluster",
      TRUE ~ "Past 2021 Endemic Cluster"),
    "DNR"=case_when(
      reporting_unit %in% c("Ageer", "Ajakdit") & str_detect(month, "November") ~1,
      reporting_unit %in% c("Ageer", "Ajakdit", "Rumdit") & str_detect(month, "October") ~1,
      TRUE~0),
    "reporting_unit_vacant_?"=case_when(
      hh_total=="0" ~ 0,
      hh_total>0 ~ 1),
    "Total_Cases_2020"=case_when(
      reporting_unit=="Achol Manak" ~ 1,
      TRUE ~ 0),
    "Total_Cases_Contained_2020"=case_when(
      reporting_unit=="Achol Manak" ~ 1,
      TRUE ~ 0),
    "Total_Cases_2021"=case_when(
      reporting_unit %in% c("Block 1", "Kengel CC", "Apukdit", "Tomrok") ~ 1,
      TRUE ~ 0),
    "Priority_Villages"=1,
    "Endemic_in_2019?"=0,
    "Endemic_Beginning_of_2020?"=0,
    "Trained Village Volunteers"=case_when(
      hh_total>0 ~ as.numeric(staff_vv),
      TRUE~NA_real_),
    "1plus_VV?"=case_when(
      staff_vv>0 & hh_total>0~1,
      hh_total=="0" ~ NA_real_,
      TRUE~0),
    "GPS?"=1,
    "Functioning_Boreholes"=case_when(
      hh_total>0 ~ as.numeric(hp_working),
      TRUE ~ NA_real_),
    "1plus_Safe_Water?"=case_when(
      hp_working>0 & hh_total>0 ~ 1,
      hh_total=="0" ~ NA_real_,
      TRUE~0),
    "Households"=case_when(
      hh_total>0 | DNR==0 ~as.numeric(hh_total),
      TRUE~NA_real_),
    "Households_with_Cloth_Filters"=case_when(
      hh_total>0~filter_hh_cloth,
      TRUE~NA_real_),
    "100_percent_Cloth_Filters?"=case_when(
      hh_total>0 & Households_with_Cloth_Filters/Households>.99~1,
      hh_total=="0" ~NA_real_,
      TRUE~0),
    "Population"=case_when(
      hh_total>0 | DNR==0 ~as.numeric(hh_total*5),
      TRUE~NA_real_),
    "Pipe_Filters_Distributed"=case_when(
      hh_total>0 ~ as.numeric(filter_dist_pipe),
      TRUE ~NA_real_),
    "Total_Water_Sources"=case_when(
      abate_targeted>0 ~ as.numeric(abate_targeted),
      TRUE ~0),
    "Targeted_Water_Sources"=Total_Water_Sources,
    "Eligible_Water_Sources"=case_when(
      abate_targeted>0 ~ abate_eligible,
      TRUE ~ 0),
    "Treated_Water_Sources"=case_when(
      abate_targeted>0 ~ abate_treated,
      TRUE ~ 0),
    "1plus_Abate_Treatment?"=case_when(
      abate_targeted>0 & abate_treated>0~1,
      abate_targeted==0 ~ 0,
      abate_targeted>0 & abate_treated==0 ~ 0,
      TRUE~0),
    #"1plus_Abate_Treatment?_(percent)"=`1plus_Abate_Treatment?`/Priority_Villages,
    "Monthly_Health_Education?"=case_when(
      hh_total>0 & activity_cra>0~1,
      hh_total=="0" ~ NA_real_,
      TRUE~0),
    #"Monthly_Health_Education?_(percent)"=`Monthly_Health_Education?`/Priority_Villages,
    "Cash_Reward_Messaging_-_Days"=case_when(
      hh_total>0 ~as.numeric(cr_days),
      TRUE~NA_real_),
    "Cash_Reward_-_Residents_Reached"=case_when(
      hh_total>0 ~ as.numeric(cr_reached),
      TRUE ~ NA_real_),
    "Video_Messaging_-_Days"=case_when(
      hh_total>0 ~ as.numeric(ed_video),
      TRUE ~ NA_real_),
    "Audio_Messaging_-_Days"=case_when(
      hh_total>0 ~ as.numeric(ed_audio),
      TRUE ~ NA_real_),
    "Integrated_Messaging_-_Days"=case_when(
      hh_total>0 ~ as.numeric(ed_integrated),
      TRUE ~ NA_real_),
    "Supervisory_Visits_by_Area_Supervisors"=as.numeric(visit_as_hw),
    "Supervisory_Visits_by_Field_Officers"= as.numeric(visit_pso_fo),
    "Supervisory_Visits_by_Field_Supervisors"=as.numeric(visit_cso_po_spo_ta),
    "Received_Monthly_Supervision?"=case_when(
      visit_as_hw+visit_pso_fo+visit_cso_po_spo_ta>0 ~1,
      TRUE~0),
    "report_abate?"=case_when(
      abate_targeted>0 ~ 1,
      TRUE ~ 0)) %>%
  select(-cr_days, -cr_reached, -ed_audio, -ed_integrated, -ed_video, -visit_as_hw, -visit_cso_po_spo_ta,
         -visit_pso_fo, -activity_cra, -cases_new, -filter_dist_pipe, -filter_hh_cloth, -hh_total, -hp_working,
         -pop_total, -staff_vv, -abate_eligible, -abate_targeted, -abate_treated) %>% 
  rename_with(~ gsub("_", " ", .x)) %>%
  rename_with(~ gsub("percent", "%", .x)) %>% 
  rename_with(~ gsub("plus", "+", .x)) %>% 
  rename_with(.cols = c(state, county, payam, `reporting unit`, month), str_to_title)

df_eci_cumulative<-df_eci_output %>% 
  select(-`Total Cases 2020`, -`Total Cases Contained 2020`, -`Total Cases 2021`, -`Priority Villages`,
         -`GPS?`) %>% 
  group_by(State, County, Payam, `Reporting Unit`, `Current or Past 2021 Endemic Cluster`, year, cc) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("Month"="YTD")

df_eci_cumulative2<-df_eci_output %>% 
  select(State, County, Payam, `Reporting Unit`, `Current or Past 2021 Endemic Cluster`,
         `Total Cases 2020`, `Total Cases Contained 2020`, `Total Cases 2021`, `Priority Villages`, `GPS?`, year, cc) %>% 
  group_by(State, County, Payam, `Reporting Unit`, `Current or Past 2021 Endemic Cluster`, year, cc) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
  mutate("Month"="YTD") %>% 
  ungroup() %>% 
  select(`Total Cases 2020`:`GPS?`) %>% 
  bind_cols(df_eci_cumulative)

df_eci_output2<-df_eci_output %>% 
  bind_rows(df_eci_cumulative2)


write_xlsx(df_eci_output2, file.path(data_out, "eci_output.xlsx"))
  


