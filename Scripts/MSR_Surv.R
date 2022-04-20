# PURPOSE: Munge and analyze MSR-Surv Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 13, 2021
# NOTES:

df_filepath_surv <- "~/Github/dewormr/Data/Databases (Final 2021)/2021 MSR-Surv database Updated .11.2.2022.Final _LJ^.xlsx"
df_filepath_hotline <- "~/Github/dewormr/Data/Databases (December 2021)/Hotline report Dec -2021 Stitched.xlsx"
df_filepath_animals <- "~/Github/dewormr/Data/Databases (December 2021)/Animal Rumors 2021_December^J2021.LJ_STITCHED.xlsx"


#MSR Data
df_msr_surv<- read_xlsx(df_filepath_surv, sheet="2021 MSR-Surv", skip=1)
df_msr_surv<-df_msr_surv %>% 
  select(-...105, -`SENIOR PROGRAM OFFICER`:-`AREA SUPERVISOR`, -`VILLAGE VOLUNTERS`, -`NAME OF CHIEF`,
         -LATITUDE, -LONGITUDE) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 2021", "NUMBER OF ASs":"NUMBER OF FEMALE ASs", 
                 "NUMBER OF VVs":"NUMBER OF FEMALE VVs"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"=replace("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)", "IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)" %in% c("cc"), NA), 
         "IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"=as.numeric("IS THE VILLAGE/CC  OCCUPIED /NOT OCCUPIED\r\n(Yes=1, No=0)"),
         month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="MSR_Surv")

#Non MSR
df_non_msr_surv<- read_xlsx(df_filepath_surv, sheet="2021-Non MSR-Surv.Jan-oct.", skip=1)
df_non_msr_surv<-df_non_msr_surv %>% 
  select(-`SENIOR PROGRAM OFFICER`:-`AREA SUPERVISOR`, -`VILLAGE VOLUNTERS`, -`NAME OF CHIEF`,
         -LATITUDE, -LONGITUDE) %>% 
  pivot_longer(c("Number of VVs - 1":"Received - 2021", "NUMBER OF ASs":"NUMBER OF FEMALE ASs", 
                 "NUMBER OF VVs":"NUMBER OF FEMALE VVs"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="Non_MSR_Surv")

#Non MSR RPIF 
df_RPIF <- openxlsx::read.xlsx(df_filepath_surv, sheet="Other Non MSR-Surv with RPIF.", startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_RPIF<-df_RPIF %>% 
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="RPIF") %>% 
  filter(PAYAM!="Grand Total",
         COUNTY!="Grand Total")

# MSR IDSR
df_IDSR<- openxlsx::read.xlsx(df_filepath_surv, sheet="2021-IDSR Rumours.", startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_IDSR<-df_IDSR %>%
  select(-"S/N") %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="IDSR") %>%
  filter(COUNTY!="TOTALS")

# Hotline Report
df_hotline<- openxlsx::read.xlsx(df_filepath_hotline, startRow=1, fillMergedCells = TRUE, sep.names=" ")
df_hotline<-df_hotline %>%
  rename_all(toupper) %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>% 
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month),  "Cumulative", month),
         source="MSR_Surv",
         sheet="hotline") %>%
  filter(COUNTY!="Totals")

# Animal Rumours
df_animal<- openxlsx::read.xlsx(df_filepath_animals, startRow=1, fillMergedCells = TRUE, sep.names=" ")
df_animal<-df_animal %>%
  select(-(starts_with("Animal Types"))) %>%
  rename_all(toupper) %>%
  pivot_longer(cols=where(is.numeric), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month),  "Cumulative", month),
         source="MSR_Surv",
         sheet="animals") %>%
  filter(COUNTY!="Totals")


