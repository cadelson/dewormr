# PURPOSE: Output a file to create pivot table for endemic cluster intervention analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 29, 2021
# NOTES: Missing reporting units: Agricultural/Wungai Cc, Apukdit, Rumdit, Rumkier
#Make sure VV numbers are from right sheet

ec_villages<-c("Block 1", "Wunethony", "Nyakhor Kamel", "Nyakhor Manyak", "Wechotda", "Kengel Cc",
               "Baragep", "Malueth Cc", "Keny Cc", "Bardhiak Cc", "Wunbul Cc", "Mayom Cc", "Tomrok",
               "Panakech", "Wungai Cc", "Apukdit", "Ajakdit", "Ageer", "Rumdit", "Rumkier")

df %>% filter(reporting_unit %in% c(ec_villages)) %>% distinct(reporting_unit)

df %>% 
  select(-vas, -cc, -name_of_water_source, -combined_merged_water_source_name, -water_source_id,
         -type_of_water_source, -ev_using_water_source, -reason_no_abate, -villages_in_reporting_unit,
         -name_of_villages_combined) %>% 
  filter(reporting_unit %in% c(ec_villages),
         indicator %in% c("cases_new", "staff_vv", "hp_working", "hh_total", "filter_hh_cloth")) %>% 
  mutate(row=row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  select(-row) %>% 
  mutate(
    "staff_vv"=as.numeric(staff_vv),
    "hp_working"=as.numeric(hp_working),
    "Total_Cases_2020"=0,
    "Total_Cases_Contained_2020"=0,
    "Cases_Contained_2020_(%)"=Total_Cases_Contained_2020/Total_Cases_2020,
    "Priority_Villages"=1,
    "Endemic_Beginning_of_2020"=0,
    "Endemic_in_2019"=0,
    "Villages_Reporting_1_Plus_Cases_in_2020"=0,
    "Villages_with_1_Plus_VV"=case_when(
      staff_vv>0~1,
      TRUE~0),
    "Villages_with_1+_VV_(%)"=Villages_with_1_Plus_VV/Priority_Villages,
    "Village_with_GPS"=1,
    "Village_with_GPS_(%)"=Village_with_GPS/Priority_Villages,
    "Village_with_1_Plus_Safe_Water"=case_when(
      hp_working>0~1,
      TRUE~0),
    "Village_with_1_Plus_Safe_Water_(%)"=Village_with_1_Plus_Safe_Water/Priority_Villages
    ) %>% 
  View()
