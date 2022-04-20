# PURPOSE: Output a file to create pivot table for rumour analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Jan 10, 2022
# NOTES:

data_out <- "~/Github/dewormr/Dataout"
rl1 <- c("Uror", "Rumbek North", "Tonj East", "Awerial")
rl2<-c("Nyirol", "Tonj North", "Tonj South", "Cueibet", "Rumbek Centre", "Mayendit",
       "Panyijiar", "Yirol East", "Yirol West", "Terekeka")
`%notin%` <- Negate(`%in%`)
months_not_this_year<-c("Cumulative")

df_rumour_output <- df_21_22 %>% 
  filter(
    #March 13, 2022 - removed month filter due to adding 2021 data
    month %notin% months_not_this_year,
         indicator %in% c("rumours_total", "suspects_total", "rumours_invest", "rumours_invest_24", 
                          "rumours_self", "rumours_informer", "cases_new", "report_expected")) %>% 
  group_by(sheet, state, county, payam, indicator, month, cc, year, vas) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from=indicator, values_from = value) %>% 
  mutate(
    # removed 3/27 - already included
    # "cc"=case_when(
    # str_detect(cc, "1") ~ "Cattle Camp",
    # TRUE ~ "Village"),
    "risk_level"=case_when(
      county %in% c(rl1) ~ "Risk Level 1",
      county %in% c(rl2) ~ "Risk Level 2",
      TRUE ~ "Risk Level 3"),
    "vas"=case_when(
      str_detect(vas, "1") ~ "VAS",
      TRUE ~ "Other"))
  #Removed all DNRs 3/14/22 
  # mutate(
  #   "filter_zeroes"=case_when(
  #     rumours_total==0 ~ "Include Non Reports",
  #     TRUE ~ "Include Submitted Reports"),
  #   "DNR"=case_when(
  #     county %in% c("Tonj South") & str_detect(month, "November") ~1,
  #     TRUE~0))

  
df_rumour_cumulative<-df_rumour_output %>% 
  group_by(state, county, payam, sheet, cc, year, vas, risk_level) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("month"="YTD")

df_rumour_output2<-df_rumour_output %>% 
  bind_rows(df_rumour_cumulative)

df_reporting_method<-df_rumour_output2 %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from=sheet, values_from=rumours_total) %>% 
  select(-row, -rumours_invest, -rumours_invest_24, -suspects_total, -cases_new, -rumours_informer, -rumours_self) %>% 
  bind_rows(df_rumour_output2) %>% 
  #3/15/22 - Removed filter zeroes and placed it in next step. Also replaced DNR and calculating with report expected
  # mutate(
    # "filter_zeroes"=case_when(
    #   rumours_total>0 | hotline>0 | IDSR > 0 | RPIF>0 | cases_new>0 ~ "Include Submitted Reports",
    #   TRUE ~ "Include Non Reports"),
    # "DNR"=case_when(
    #   county %in% c("Tonj South") & str_detect(month, "November") ~1,
    #   TRUE~0)) %>% 
  filter(!is.na(state) | !is.na(county) | !is.na(payam))

#To add filter option, collapse data frame and then merge with output on month and payam
df_filter_non_reporting<-df_reporting_method %>% 
  group_by(county, payam, month, year) %>% 
  summarise(across(c(rumours_total, hotline, IDSR, RPIF, cases_new, report_expected), sum, na.rm=TRUE)) %>% 
  mutate(
    "filter_zeroes"=case_when(
      rumours_total>0 | hotline>0 | IDSR > 0 | RPIF>0 | cases_new>0 ~ "Include Submitted Reports",
      TRUE ~ "Include Non Reports"),
    "reporting_expected"=case_when(
      year==2022 & report_expected>0 ~ 1,
      TRUE~ 0)) %>% 
  select(county, payam, month, year, filter_zeroes, reporting_expected)

df_reporting_method <- df_reporting_method %>% 
  left_join(df_filter_non_reporting)

write_xlsx(df_reporting_method, file.path(data_out, "rumour_output.xlsx"))
