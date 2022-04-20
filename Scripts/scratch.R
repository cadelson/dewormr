# PURPOSE: Scratch
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: February 2, 2022
# NOTES:

df_21 %>% 
  filter(month==current_month,
         county %in% c(level_3),
         vas=="1",
         indicator %in% c("report_expected")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))

df_21 %>% 
  filter(month!="Cumulative",
         vas=="1",
         indicator %in% c("report_expected", "report_received")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))

df %>% 
  filter(indicator=="abate_targeted",
         month=="January") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))

df %>% 
  filter(vas=="1",
         #indicator %in% c("report_expected", "report_received"),
         indicator %in% c("report_received"),
         value>0,
         month=="December") %>% 
         #month!="Cumulative") %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c("value"), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(vas=="1",
         #indicator %in% c("report_expected", "report_received"),
         indicator %in% c("hh_total", "filter_hh_cloth"),
         month=="December") %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(coverage=filter_hh_cloth/hh_total) %>% 
  filter(coverage>.95) %>% 
  #month!="Cumulative") %>% 
  group_by(cc) %>% 
  count() %>% 
  View()

df %>% 
  filter(vas=="1",
         #indicator %in% c("report_expected", "report_received"),
         indicator %in% c("activity_cra"),
         month=="December",
         value>0) %>% 
  #month!="Cumulative") %>% 
  group_by(indicator, cc) %>% 
  summarise(across(c("value"), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(indicator %in% c("abate_targeted", "abate_eligible")) %>% 
  group_by(county, reporting_unit, indicator, month) %>% 
  summarise(across(c("value"), sum, na.rm=TRUE)) %>% 
  filter(value>0) %>% 
  group_by(indicator) %>% 
  count(reporting_unit) %>% 
  summarise(across(c("n"), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(month=="December", 
         vas=="1",
         source=="MSR_Surv") %>% 
  distinct(reporting_unit) %>% 
  count()

df %>% 
  filter(month=="December", 
         vas=="1",
         source=="MSR_Surv") %>% 
  distinct(payam, county, reporting_unit) %>% 
  count()

df %>% 
  filter(month=="December", 
         vas=="1",
         indicator=="report_expected",
         value==1,
         source=="MSR_Surv") %>% 
  distinct(reporting_unit_code) %>% 
  count()

df %>% 
  filter(month=="January", 
         vas=="1",
         indicator=="report_expected",
         value==1,
         source=="MSR_Surv") %>% 
  distinct(reporting_unit_code) %>% 
  count()

df %>% 
  filter(month=="December", 
         vas=="1",
         indicator=="report_expected",
         value==1,
         source=="MSR_Surv") %>% 
  summarise(across(c(value), sum, na.rm=TRUE))

df %>% 
  filter(indicator %in% c("staff_vv", "pop_total"),
         month=="December",
         county %in% c("Uror", "Rumbek North", "Tonj East", "Nyirol")) %>% 
  group_by(county, indicator, month, sheet, reporting_unit) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from=indicator, values_from = value) %>% 
  View()


df %>% 
  filter(indicator=="staff_vv",
         month=="December",
         county %in% c("Uror", "Rumbek North", "Tonj East", "Nyirol")) %>% 
  group_by(county, indicator, month, sheet) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(vas=="1",
         month=="December",
         payam=="Bunagok",
         indicator %in% c("report_expected", "report_received")) %>% 
  View()

mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  group_by(state, county, payam, month) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  select(-vas, -villages_in_reporting_unit, -cc_occupied) %>% 
  View()

df_rough %>% 
  filter(PAYAM=="BUNAGOK",
         month=="December") %>% 
  View()


#Check reports received
# df %>%
#   filter(indicator %in% c("report_expected", "report_received"),
#          month %notin% c("Cumulative", "December"),
#          reporting_unit!="Wech Nyhal",
#          vas=="1") %>%
#   select(state, county, payam, boma, reporting_unit, indicator, month, value, sheet) %>%
#   mutate(row=row_number()) %>%
#   pivot_wider(names_from=indicator, values_from=value) %>%
#   select(-row) %>%
#   group_by(state, county, payam, month, reporting_unit, sheet) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   filter(report_expected>report_received) %>%
#   write_xlsx(file.path(data_out, "non_reporting_reporting_unit_vas.xlsx"))
# 
# df %>%
#   filter(indicator %in% c("report_expected", "report_received"),
#          month %notin% c("Cumulative", "December"),
#          reporting_unit!="Wech Nyhal") %>%
#   select(state, county, payam, boma, reporting_unit, indicator, month, value, sheet, vas) %>%
#   mutate(row=row_number()) %>%
#   pivot_wider(names_from=indicator, values_from=value) %>%
#   select(-row) %>%
#   group_by(vas, state, county, payam, reporting_unit, month, sheet) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
#   filter(report_expected>report_received) %>%
#   write_xlsx(file.path(data_out, "non_reporting_reporting_unit.xlsx"))
month_order<- c("January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December", "Cumulative")

df %>% filter(
  payam=="Pieri", 
  indicator %in% c("filter_dist_cloth")) %>% 
  group_by(payam, indicator, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(month=fct_relevel(month, month_order)) %>% 
  arrange(month) %>% 
  View()


fct_relevel("January", "Feburary") %>% 
  View()

df %>% 
  filter(indicator=="suspects_total") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(indicator=="report_expected",
         month=="December") %>% 
  group_by(county, indicator, vas, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(indicator=="report_expected",
         month=="December") %>% 
  group_by(indicator, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()


# df_eci_output2 %>% 
#   filter(`Reporting Unit` =="Ageer",
#          Month=="November") %>% 
#   View()
# 
# df %>% 
#   filter(indicator=="visit_as_hw",
#          month=="November",
#          reporting_unit=="Ageer") %>% 
#   View()
#   group_by(reporting_unit) %>% 
#   summarise(across(c(value), sum, na.rm = TRUE)) 

df %>% 
  filter(reporting_unit=="Wunbul Cc",
         month=="November",
         indicator=="pop_total") %>% 
  View()


df_msr_surv_1 %>% 
  filter(PAYAM=="BUNAGOK",
         month=="December") %>% 
  View()
indicator %in% c("Expected", "Received")) %>% 
  View()
