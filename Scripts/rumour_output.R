# PURPOSE: Output a file to create pivot table for rumour analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Jan 10, 2022
# NOTES:

data_out <- "~/Github/dewormr/Dataout"
`%notin%` <- Negate(`%in%`)
months_not_this_year<-c("December", "Cumulative")

df_rumour_output <- df %>% 
  filter(month %notin% months_not_this_year,
         indicator %in% c("rumours_total", "suspects_total", "rumours_invest", "rumours_invest_24", 
                          "rumours_self", "rumours_informer", "cases_new")) %>% 
  group_by(sheet, state, county, payam, indicator, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from=indicator, values_from = value) %>% 
  mutate(
    "filter_zeroes"=case_when(
      rumours_total==0 ~ "Include Non Reports",
      TRUE ~ "Include Submitted Reports"),
    "DNR"=case_when(
      county %in% c("Tonj South") & str_detect(month, "November") ~1,
      TRUE~0))

  
df_rumour_cumulative<-df_rumour_output %>% 
  group_by(state, county, payam, sheet, filter_zeroes) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("month"="YTD")

df_rumour_output2<-df_rumour_output %>% 
  bind_rows(df_rumour_cumulative)

write_xlsx(df_rumour_output2, file.path(data_out, "rumour_output.xlsx"))
