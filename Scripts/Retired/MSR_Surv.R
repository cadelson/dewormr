# PURPOSE: Munge and analyze MSR-Surv Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 13, 2021
# NOTES:

df_filepath_surv_21 <- "~/Github/dewormr/Data/Databases (Final 2021)/2021 MSR-Surv database Updated .11.2.2022.Final _LJ^.xlsx"
df_filepath_hotline_21 <- "~/Github/dewormr/Data/Databases (Final 2021)/Hotline report Final -2021 Stitched.xlsx"
df_filepath_animals_21 <- "~/Github/dewormr/Data/Databases (Final 2021)/Animal Rumors 2021_Final^J2021.LJ_STITCHED.xlsx"


#MSR Data
df_msr_surv<- read_xlsx(df_filepath_surv_21, sheet="2021 MSR-Surv", skip=1)
df_msr_surv<-df_msr_surv %>% 
  select(-...105, -`NUMBER OF VILLAGES PER REPORTING UNIT`:-`NAME OF CHIEF`, -`Number of VVs - 2021`:-`Received - 2021`) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="MSR_Surv")


#Non MSR
df_non_msr_surv<- read_xlsx(df_filepath_surv_21, sheet="2021-Non MSR-Surv.Jan-oct.", skip=1)
df_non_msr_surv<-df_non_msr_surv %>% 
  select(-`NUMBER OF VILLAGES PER REPORTING UNIT`:-`NAME OF CHIEF`, -`Number of VVs - 2021`:-`Received - 2021`) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 12"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="Non_MSR_Surv")

#Non MSR RPIF 
df_RPIF <- openxlsx::read.xlsx(df_filepath_surv_21, sheet="Other Non MSR-Surv with RPIF.", startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_RPIF<-df_RPIF %>% 
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="RPIF") %>% 
  filter(PAYAM!="Grand Total",
         COUNTY!="Grand Total")

# MSR IDSR
df_IDSR<- openxlsx::read.xlsx(df_filepath_surv_21, sheet="2021-IDSR Rumours.", startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_IDSR<-df_IDSR %>%
  select(-"S/N", -`NO.RUMOURS-Totals`:-`SUSPECTS - Totals`  ) %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="IDSR") %>%
  filter(COUNTY!="TOTALS")

# Hotline Report
df_hotline<- openxlsx::read.xlsx(df_filepath_hotline_21, startRow=1, fillMergedCells = TRUE, sep.names=" ")
df_hotline<-df_hotline %>%
  rename_all(toupper) %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>% 
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="hotline") %>%
  filter(COUNTY!="Totals")

# Animal Rumours
df_animal<- openxlsx::read.xlsx(df_filepath_animals_21, startRow=1, fillMergedCells = TRUE, sep.names=" ")
df_animal<-df_animal %>%
  select(-(starts_with("Animal Types"))) %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=month.name[as.numeric(month)],
         source="MSR_Surv",
         sheet="animals") %>%
  filter(COUNTY!="Totals")


