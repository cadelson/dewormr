# PURPOSE: Analyze dataset for monthly NEMO submission
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 23, 2021
# Notes: May 2022 - update to change order in order of prompts
# ALSO FOR TOTAL NUMBER OF VAS, USE DISTINCT RATHER THAN ADDING DUE TO DUPLICATES (SEE SCRATCH AS TEMPLATE)

current_month<-("November")
current_year<-("2024")
villages_1plus <- c("Tomrok", "Apukdit", "Kengel CC", "Block 1")
villages_1plus <- c("Tomrok", "Apukdit", "Wunlaac", "Jarweng", "Abongbeo")
`%notin%` <- Negate(`%in%`)

#Number of human GWD rumours reported for VAS/NVAs 
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         vas==1,
         indicator %in% c("rumours_total", "rumours_invest_24"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")) %>% 
  #group_by(month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE), .by = c(month, indicator, cc))
#Number of ANIMAL GWD rumours reported for VAS/NVAs 
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet %in% c("animals")) %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of VAS and Received LEVEL ONE, TWO, THREE
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         vas=="1",
         indicator %in% c("report_expected", "report_received")) %>% 
  group_by(indicator, cc, risk_level) %>% 
  # NEED TO FILTER EXPECTED ==1 before tally, etc, reshape?
  # distinct(state, county, payam, boma, supervisory_area, reporting_unit) %>% 
  # tally()
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of 1+ Villages/CCs
df_21_22 %>% 
  filter(indicator=="cases_new",
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(reporting_unit, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  filter(value>0)
#Number of 1+ Villages/CCs with health education each month
df_21_22 %>% 
  filter(indicator %in% c("activity_cra"),
         year==current_year,
         month==current_month,
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(reporting_unit, month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  filter(activity_cra>0)
#Number of VAS/NVAs receiving at least one health education session each month (level 1 and 2)
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         vas=="1",
         indicator=="activity_cra",
         risk_level %in% c("Risk Level 1", "Risk Level 2"),
         value>0) %>%
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of VAS/NVAs with no access to safe drinking water
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         indicator=="hp_working",
         vas=="1",
         value==0) %>%
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam, county) %>% 
  count()
#VAS with no safe drinking water and 100% filter coverage
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         vas=="1",
         indicator %in% c("hp_working", "hh_total", "filter_hh_cloth")) %>% 
  pivot_wider(names_from = indicator, values_from=value) %>% 
  filter(hh_total>0,
         hp_working==0) %>% 
  mutate(hh_coverage=filter_hh_cloth/hh_total) %>% 
  filter(hh_coverage>.9999) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of 1+ villages/CCs with no access to safe drinking water
df_21_22 %>% 
  filter(indicator %in% c("cases_new", "hp_working"),
         month==current_month,
         year==current_year,
         vas==1,
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(county, reporting_unit, month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from=value) %>% 
  filter(hp_working==0) %>% 
  group_by(cc) %>% 
  distinct(county, reporting_unit) %>%
  View()
  count()
#Number of 1+ villages/CCs with no access to safe drinking water with 100% CF
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         vas==1,
         reporting_unit %in% c(villages_1plus),
         indicator %in% c("hp_working", "hh_total", "filter_hh_cloth")) %>% 
  pivot_wider(names_from = indicator, values_from=value) %>% 
  filter(hh_total>0,
         hp_working==0) %>% 
  mutate(hh_coverage=filter_hh_cloth/hh_total) %>% 
  filter(hh_coverage>.9999) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  View()
  count()
#Number of VAS with trained and supervised village volunteers
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         indicator=="staff_vv",
         vas==1,
         value>0) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()

#1+ villages with trained and supervised VVS
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         reporting_unit %in% c(villages_1plus),
         payam %notin% c("Wuror", "Pathai"),
         indicator=="staff_vv",
         value>0,
         vas==1) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>%
  View()
  count()
#Number of VAS conducting weekly case search (level I and II)
df_21_22 %>% 
  filter(month==current_month,
         year==current_year,
         indicator=="activity_case_search",
         risk_level %in% c("Risk Level 1", "Risk Level 2"),
         value>0,
         vas==1) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of water sources in 1+villages targeted for ABATE application
df_21_22 %>% 
  filter(reporting_unit %in% c(villages_1plus),
         indicator %in% c("abate_targeted", "abate_eligible", "abate_treated"),
         month==current_month,
         year==current_year,
         value>0) %>% 
  group_by(indicator, cc) %>% 
  distinct(name_of_water_source) %>% 
  count()
#Number of VAS that are NVAs in Level 1/2/3
df_22_24 %>% 
  filter(month==current_month,
         year==current_year,
         vas=="1",
         #cc=="Cattle Camp",
         indicator %in% c("report_expected")) %>% 
  group_by(indicator, #cc, 
           #risk_level
           ) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
