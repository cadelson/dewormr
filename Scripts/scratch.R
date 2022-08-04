# PURPOSE: Scratch
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: February 2, 2022
# NOTES:

#Uror filter coverage for provisional analysis
df_21_22 %>% 
  filter(indicator %in% c("filter_hh_cloth", "hh_total", "report_expected", "report_received"),
         month %in% c("January", "February", "March", "April", "May", "June"),
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
