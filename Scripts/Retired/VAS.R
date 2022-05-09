# PURPOSE: Prepare analysis for villages under active surveillance monthly deliverable
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 14, 2021
# NOTES: 

df_vas<-df %>% 
  filter(vas==1,
         indicator %in% c("report_expected", "report_received", "cases_new", "cases_contained",
                          "cases_imported"),
         month=="October") %>% 
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup()
  pivot_wider(names_from=indicator, values_from=value) %>%
  mutate(reporting_percent=report_received/report_expected,
         contained_percent=cases_contained/cases_new,
         cases_indigenous=cases_new-cases_imported) %>%
  relocate(state, county, payam, report_expected, report_received, reporting_percent, cases_new, cases_contained,
           contained_percent, cases_imported, cases_indigenous) %>% 
  View()


  df_vas[, .(total = sum(value)), by = c("indicator")]

df_vas<-df %>% 
  filter(vas==1,
         indicator %in% c("report_expected", "report_received", "cases_new", "cases_contained",
                          "cases_imported"),
         month=="October") %>% 
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>% 
  df_vas[, sum(value), by = c("state", "county", "payam", "indicator")]
  
  
  
  
  pivot_wider(names_from=indicator, values_from=value) %>%
  mutate(reporting_percent=report_received/report_expected,
         contained_percent=cases_contained/cases_new,
         cases_indigenous=cases_new-cases_imported) %>%
  relocate(state, county, payam, report_expected, report_received, reporting_percent, cases_new, cases_contained,
           contained_percent, cases_imported, cases_indigenous) %>% 
  View()