# PURPOSE: Output a file to create pivot table for Village Under Active Surveillance analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Jan 27, 2022
# NOTES:

data_out <- "~/Github/dewormr/Dataout"
`%notin%` <- Negate(`%in%`)

df_vas_output<-df_21_22 %>% 
  filter(vas=="1",
         indicator %in% c("report_expected", "report_received", "cases_new", "cases_contained", "cases_imported")) %>% 
  mutate(value=as.numeric(value),
         row = row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  group_by(state, county, payam, month, year, cc, risk_level) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  select(-row, -vas)

df_vas_cumulative<-df_vas_output %>%
  group_by(state, county, payam, cc, risk_level, year) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("month"="YTD")

df_vas_output<-df_vas_output %>% 
  bind_rows(df_vas_cumulative)

write_xlsx(df_vas_output, file.path(data_out, "vas_output.xlsx"))  

#Summarise total missing reports by risk level for monthly narrative
df_21_22 %>%
  filter(month=="December",
         year=="2022",
         sheet=="MSR_Surv",
         indicator %in% c("report_expected", "report_received")) %>%
  mutate(value=as.numeric(value),
         row=row_number()) %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  group_by(state, county, payam, month, risk_level) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  select(-vas, -row) %>%
  filter(report_expected>report_received) %>%
  arrange(-desc(risk_level)) %>% 
  ungroup() %>% 
  mutate(" "=row_number(),
         "State"=state,
         "County"=county,
         "Payam"=payam,
         "Risk Level"=risk_level) %>% 
  select(" ", State, County, Payam, "Risk Level") %>% 
  write_xlsx(file.path(data_out, "december_2022_missing_reports.xlsx"))
