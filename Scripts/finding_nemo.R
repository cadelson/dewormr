# PURPOSE: Analyze combined dataset for NEMO
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 23, 2021
# NOTES:

current_month<-("November")
level_1 <- c("Uror", "Awerial", "Tonj East", "Rumbek North")
level_2<- c("Akobo", "Nyirol", "Tonj South", "Tonj North", "Rumbek Centre")
level_3<- c("Wau", "Jur River", "Torit", "Lopa/Lafon", "Kapoeta East", "Kapoeta South",
            "Kapoeta North", "Riaga", "Ikotos", "Wulu", "Rumbek East")
villages_1plus <- c("Tomrok", "Block 1", "Apukdit", "Kengel Cc")

#Number of new human GWD cases for VAS
df %>%
  filter(month==current_month,
         indicator=="cases_new",
         reporting_unit %in% c(villages_1plus),
         cc=="0") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of new GWD cases for NVAs
df %>%
  filter(month==current_month,
         reporting_unit %in% c(villages_1plus),
         indicator=="cases_new",
         cc=="1") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) 
#Number of new CONTAINED human GWD cases for VAS
df %>%
  filter(month==current_month,
         reporting_unit %in% c(villages_1plus),
         indicator=="cases_contained",
         cc=="0") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of new CONTAINED human GWD cases for NVAS
df %>%
  filter(month==current_month,
         reporting_unit %in% c(villages_1plus),
         indicator=="cases_contained",
         cc=="1") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of human GWD rumours reported for VAS/NVAs 
df %>% 
  filter(month==current_month,
         vas=="1",
         indicator %in% c("rumours_total", "rumours_invest_24"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of ANIMAL GWD rumours reported for VAS/NVAs 
df %>% 
  filter(month==current_month,
         indicator %in% c("rumours_total", "rumours_invest_24"),
         sheet %in% c("animals")) %>% 
  group_by(month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of VAS and Received LEVEL ONE
df %>% 
  filter(month==current_month,
         vas=="1",
         indicator %in% c("report_expected", "report_received"),
         county %in% c(level_1)) %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of VAS and Received LEVEL TWO
df %>% 
  filter(month==current_month,
         vas=="1",
         indicator %in% c("report_expected", "report_received"),
         county %in% c(level_2)) %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of VAS and Received LEVEL THREE
df %>% 
  filter(month==current_month,
         vas=="1",
         indicator %in% c("report_expected", "report_received"),
         county %in% c(level_3)) %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
#Number of 1+ Villages/CCs
df %>% 
  filter(indicator=="cases_new",
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(reporting_unit, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  filter(value>0)
#Number of 1+ Villages/CCs with health education each month
df %>% 
  filter(indicator %in% c("cases_new", "activity_cra"),
         #month==current_month,
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(reporting_unit, month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from=value) %>% 
  filter(activity_cra>0)
#Number of VAS/NVAs receiving at least one health education session each month (level 1 and 2)
df %>% 
  filter(month==current_month,
         indicator=="activity_cra",
         county %in% c(level_1, level_2),
         value>0) %>%
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of VAS/NVAs with no access to safe drinking water
df %>% 
  filter(month==current_month,
         indicator=="hp_working",
         value==0) %>%
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#VAS with no safe drinking water and 100% filter coverage
df %>% 
  filter(month==current_month,
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
df %>% 
  filter(indicator %in% c("cases_new", "hp_working"),
         month==current_month,
         vas==1,
         reporting_unit %in% c(villages_1plus)) %>% 
  group_by(reporting_unit, month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from=value) %>% 
  filter(hp_working==0) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit) %>% 
  count()
#Number of 1+ villages/CCs with no access to safe drinking water with 100% CF
df %>% 
  filter(month==current_month,
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
  count()
#Number of 1+ villages/CCs protected with Abate during transmission periods
df %>% 
  filter(reporting_unit %in% c(villages_1plus),
         indicator %in% c("abate_treated")) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of VAS with trained and supervised village volunteers
###FOLLOW UP IF THIS SHOULD BE JUST FOR VAS?
df %>% 
  filter(month==current_month,
         indicator=="staff_vv",
         value>0) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
####################VVS FOR ADAM
df %>% 
  filter(indicator %in% c("staff_vv", "staff_vv_male", "staff_vv_female", "staff_as",
                          "staff_as_male", "staff_as_female")) %>% 
  group_by(indicator, sheet, month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()
  
#1+ villages with trained and supervised VVS
df %>% 
  filter(month==current_month,
         reporting_unit %in% c(villages_1plus),
         indicator=="staff_vv",
         value>0,
         vas==1) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count
#Number of VAS conducting weekly case search (level I and II)
df %>% 
  filter(month==current_month,
         indicator=="activity_case_search",
         county %in% c(level_1, level_2),
         value>0,
         vas==1) %>% 
  group_by(cc) %>% 
  distinct(reporting_unit, boma, payam) %>% 
  count()
#Number of 1+ villages targeted for ABATE (in transmission period and with at least one eligible water source)
df %>% 
  filter(reporting_unit %in% c(villages_1plus),
         indicator %in% c("abate_targeted", "abate_eligible")) %>% 
  group_by(cc, month) %>% 
  group_by(reporting_unit, month, indicator, cc) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  pivot_wider(names_from=indicator, values_from=value) %>% 
  filter(abate_eligible>0,
         abate_targeted>0)
#Number of water sources in 1+villages targeted for ABATE application
df %>% 
  filter(reporting_unit %in% c(villages_1plus),
         indicator %in% c("abate_targeted", "abate_eligible", "abate_treated"),
         month==current_month,
         value>0) %>% 
  group_by(indicator) %>% 
  distinct(name_of_water_source) %>% 
  count()
  
###################SUPERVISORY VISITS FOR ADAM
df %>% 
  filter(state %in% c("Warrap", "Western Bahr El Ghazal", "Jonglei", "Eastern Equatoria" ),
         indicator %in% c("visit_as_hw", "visit_pso_fo", "visit_cso_po_spo_ta", "visit_secretariat"),
         month!="Cumulative") %>%
  summarise(across(c(value), sum, na.rm=TRUE))
