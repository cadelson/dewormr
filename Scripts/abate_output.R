# PURPOSE: Output a file to create pivot table for abate analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 27, 2021
# NOTES:
data_out <- "~/Github/dewormr/Dataout"
months_not_this_year<-c("December")

df_abate_output<-df %>% 
  filter(sheet=="Abate",
         month!=months_not_this_year) %>% 
  mutate("Negative CDC Result"=case_when(str_detect(reason_no_abate, "Negative") ~ "1", TRUE~"0"),
         "Negative CDC Result"=as.numeric(`Negative CDC Result`),
         "Other"=case_when(str_detect(reason_no_abate, "Other") ~ "1", TRUE~"0"),
         "Other"=as.numeric(`Other`),
         "Dry" =case_when(str_detect(reason_no_abate, "Dry") ~ "1", TRUE~"0"),
         "Dry"=as.numeric(`Dry`),
         "Flooded"=case_when(str_detect(reason_no_abate, "Flooded") ~ "1", TRUE~"0"),
         "Flooded"=as.numeric(`Flooded`),
         "Flowing"=case_when(str_detect(reason_no_abate, "Flowing") ~ "1", TRUE~"0"),
         "Flowing"=as.numeric(`Flowing`),
         "Insecurity"=case_when(str_detect(reason_no_abate, "Insecurity") ~ "1", TRUE~"0"),
         "Insecurity"=as.numeric(`Insecurity`)) %>% 
  group_by(state, county, payam, month, indicator) %>% 
  summarise(across(c("value", `Negative CDC Result`, `Other`, `Dry`, `Flooded`, `Flowing`, `Insecurity`), sum, na.rm = TRUE)) %>%
  pivot_wider(names_from=indicator, values_from=value) %>% 
  mutate(
    "Total Water Sources"=abate_targeted,
    "Targeted Water Sources"=abate_targeted,
    "Eligible Water Sources"=abate_eligible,
    "Treated Water Sources"=abate_treated,
    "Ineligible Water Sources"=abate_targeted-abate_eligible,
    "DNR"=case_when(
      str_detect(county, "Tonj East") & str_detect(month, "November") ~"1",
      str_detect(payam, "Thiet") & str_detect(month, "September") ~"1",
      str_detect(county, "Tonj South") & str_detect(month, "August") ~"1",
      TRUE~"0")) %>% 
  select(-abate_targeted, -abate_eligible, -abate_treated, -abate_reason_untreated) %>%
  rename(
    "State"=state,
    "County"=county,
    "Payam"=payam,
    "Month"=month)

df_abate_cumulative<-df_abate_output %>% 
  group_by(State, County, Payam) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("Month"="YTD")

df_abate_output2<-df_abate_output %>% 
  bind_rows(df_abate_cumulative)
  

write_xlsx(df_abate_output2, file.path(data_out, "abate_output.xlsx"))




