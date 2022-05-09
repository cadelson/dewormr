# PURPOSE: Output a file to create pivot table for abate analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 27, 2021
# NOTES:
data_out <- "~/Github/dewormr/Dataout"

#April 2022- new risk level variable, not needed
# rl1 <- c("Uror", "Rumbek North", "Tonj East", "Awerial")
# rl2<-c("Nyirol", "Tonj North", "Tonj South", "Cueibet", "Rumbek Centre", "Mayendit",
#        "Panyijiar", "Yirol East", "Yirol West", "Terekeka")

df_abate_output<-df_21_22 %>% 
  filter(sheet=="Abate") %>% 
  mutate("Negative CDC Result"=case_when(str_detect(reason_no_abate, "Negative") ~ 1, TRUE~0),
         "Other"=case_when(str_detect(reason_no_abate, "Other") ~ 1, TRUE~0),
         "Dry" =case_when(str_detect(reason_no_abate, "Dry") ~ 1, TRUE~0),
         "Flooded"=case_when(str_detect(reason_no_abate, "Flooded") ~ 1, TRUE~0),
         "Flowing"=case_when(str_detect(reason_no_abate, "Flowing") ~ 1, TRUE~0),
         "Insecurity"=case_when(str_detect(reason_no_abate, "Insecurity") ~ 1, TRUE~0)) %>% 
  group_by(state, county, payam, month, indicator, cc, year, risk_level) %>% 
  summarise(across(c("value", `Negative CDC Result`, `Other`, `Dry`, `Flooded`, `Flowing`, `Insecurity`), sum, na.rm = TRUE)) %>%
  pivot_wider(names_from=indicator, values_from=value) %>% 
  #April 2022 - So far, other water sources are all not targeted, must continue to be the case for below mutate, next month make it consistent and send out email
  mutate(
    "Total Water Sources"=case_when(
      year=="2021" ~ abate_targeted,
      year=="2022" ~ abate_targeted+`Negative CDC Result`+`Other`),
    "Targeted Water Sources"=abate_targeted,
    "Eligible Water Sources"=abate_eligible,
    "Treated Water Sources"=abate_treated,
    "Ineligible Water Sources"=abate_targeted-abate_eligible,
    "DNR"=case_when(
      str_detect(payam, "Thiet") & str_detect(month, "September") ~1,
      TRUE ~ 0))  %>%
  select(-abate_targeted, -abate_eligible, -abate_treated, -abate_reason_untreated) %>%
  rename_with(.cols = c(state, county, payam, month), str_to_sentence)

df_abate_cumulative<-df_abate_output %>% 
  group_by(State, County, Payam, cc, risk_level, year) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("Month"="YTD")


df_abate_output2<-df_abate_output %>% 
  bind_rows(df_abate_cumulative)
  

write_xlsx(df_abate_output2, file.path(data_out, "abate_output.xlsx"))
