# PURPOSE: Munge and analyze MSR-CR Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 14, 2021
# NOTES:

df_filepath_cr_21 <- "~/Github/dewormr/Data/Databases (Final 2021)/2021 MSR-CR Database _Final. 15-02-2022.LJ^.xlsx"

#MSR Data
df_msr_cr<- read_xlsx(df_filepath_cr_21, sheet="2021 MSR-CR", skip=0)
df_msr_cr <- df_msr_cr %>% 
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`NAME OF CHIEF`, -`Total Number of Residents Reached with Cash Reward Messages - 2021`: -...183) %>% 
  pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_CR",
         sheet="MSR_CR")

#Non MSR
df_non_msr_cr<- read_xlsx(df_filepath_cr_21, sheet="Non -MSR-CR ,Database.Jan-Oct.", skip=1)
df_non_msr_cr <- df_non_msr_cr %>% 
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`NAME OF CHIEF`, -`Total Number of Residents Reached with Cash Reward Messages - 2021`: -`SSGWEP Secretariat - 2021`) %>%
  pivot_longer(c(`Total Number of Residents Reached with Cash Reward Messages - 1` :`SSGWEP Secretariat - 12`), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_CR",
         sheet="Non_MSR_CR")
