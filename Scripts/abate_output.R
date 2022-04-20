# PURPOSE: Output a file to create pivot table for abate analysis
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 27, 2021
# NOTES:
data_out <- "~/Github/dewormr/Dataout"
months_not_this_year<-c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
rl1 <- c("Uror", "Rumbek North", "Tonj East", "Awerial")
rl2<-c("Nyirol", "Tonj North", "Tonj South", "Cueibet", "Rumbek Centre", "Mayendit",
       "Panyijiar", "Yirol East", "Yirol West", "Terekeka")

df_abate_output<-df_21_22 %>% 
  filter(sheet=="Abate") %>% 
        #month!=months_not_this_year) %>% 
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
  group_by(state, county, payam, month, indicator, cc, year) %>% 
  summarise(across(c("value", `Negative CDC Result`, `Other`, `Dry`, `Flooded`, `Flowing`, `Insecurity`), sum, na.rm = TRUE)) %>%
  pivot_wider(names_from=indicator, values_from=value) %>% 
  mutate(
    "Total Water Sources"=case_when(
      year=="2021" ~ abate_targeted,
      year=="2022" ~ abate_targeted+`Negative CDC Result`),
    "Targeted Water Sources"=abate_targeted,
    "Eligible Water Sources"=abate_eligible,
    "Treated Water Sources"=abate_treated,
    "Ineligible Water Sources"=abate_targeted-abate_eligible,
    "DNR"=case_when(
      str_detect(payam, "Thiet") & str_detect(month, "September") ~1,
      TRUE ~ 0),
    #Removed 3/27 - this is already done in a previous step
    # "cc"=case_when(
    #   str_detect(cc, "1") ~ "Cattle Camp",
    #   TRUE ~ "Village"),
    "risk_level"=case_when(
      county %in% c(rl1) ~ "Risk Level 1",
      county %in% c(rl2) ~ "Risk Level 2",
      TRUE ~ "Risk Level 3")) %>%
  select(-abate_targeted, -abate_eligible, -abate_treated, -abate_reason_untreated) %>%
  rename_with(.cols = c(state, county, payam, month), str_to_sentence)

df_abate_cumulative<-df_abate_output %>% 
  group_by(State, County, Payam, cc, risk_level, year) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate("Month"="YTD")



df_abate_output2<-df_abate_output %>% 
  bind_rows(df_abate_cumulative)
  

write_xlsx(df_abate_output2, file.path(data_out, "abate_output.xlsx"))




