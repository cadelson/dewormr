# PURPOSE: Munge and analyze MSR-CR Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 14, 2021
# NOTES:

df_filepath_cr <- "~/Github/dewormr/Data/Databases (November 2021)/2021 MSR-CR Database _Final. 17-12-2021.LJ..xlsx"

#MSR Data
df_msr_cr<- read_xlsx(df_filepath_cr, sheet="2021 MSR-CR", skip=0)
df_msr_cr_1 <- df_msr_cr %>% 
  select(-...183, -LATITUDE:-"AREA SUPERVISOR", -"VILLAGE VOLUNTERS", -"NAME OF CHIEF") %>% 
  pivot_longer(c("NUMBER OF ASs":"NUMBER OF FEMALE ASs", "NUMBER OF VVs":"NUMBER OF FEMALE VVs",
                 "Total Number of Residents Reached with Cash Reward Messages - 1":"SSGWEP Secretariat - 2021"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"=replace("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)", "IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)" %in% c("cc"), NA), 
                 "IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"=as.numeric("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"),
         month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_CR",
         sheet="MSR_CR")

#Non MSR
df_non_msr_cr<- read_xlsx(df_filepath_cr, sheet="Non -MSR-CR ,Database.Jan-Oct.", skip=1)
df_non_msr_cr_1 <- df_non_msr_cr %>% 
  select(-LATITUDE:-"AREA SUPERVISOR", -"VILLAGE VOLUNTERS", -"NAME OF CHIEF") %>% 
  pivot_longer(c("NUMBER OF ASs":"NUMBER OF FEMALE ASs", "NUMBER OF VVs":"NUMBER OF FEMALE VVs",
                 "Total Number of Residents Reached with Cash Reward Messages - 1":"SSGWEP Secretariat - 2021"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_CR",
         sheet="Non_MSR_CR")
