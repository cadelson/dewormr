# PURPOSE: Munge and analyze MSR-Surv Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 13, 2021
# NOTES:

df_filepath_surv <- "~/Github/dewormr/Data/2021 MSR-Surv database.25.11.2021.Final _LJ..xlsx"
df_filepath_hotline <- "~/Github/dewormr/Data/hotline report Oct-2021.xlsx"
df_filepath_animals <- "~/Github/dewormr/Data/Animal Rumors 2021_OCT.xlsx"


#MSR Data
df_msr_surv<- read_xlsx(df_filepath_surv, sheet="2021 MSR-Surv", skip=1)
df_msr_surv_1<-df_msr_surv %>% 
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
df_non_msr_surv_1<-df_non_msr_surv %>% 
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
df_RPIF2<-df_RPIF %>% 
  pivot_longer(c("Total Number of Rumours - 5":"Suspects-10"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="RPIF") %>% 
  filter(PAYAM!="Grand Total")

# MSR IDSR Ensure no double counting
df_IDSR<- openxlsx::read.xlsx(df_filepath_surv, sheet="2021-IDSR Rumours.", startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_IDSR2<-df_IDSR %>%
  select(-"S/N") %>%
  pivot_longer(c("NO.RUMOURS - 7":"SUSPECTS - 10"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="MSR_Surv",
         sheet="IDSR") %>%
  filter(COUNTY!="TOTALS")

# Hotline Report Ensure no double counting
df_hotline<- openxlsx::read.xlsx(df_filepath_hotline, startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_hotline2<-df_hotline %>%
  select(-"S/N") %>%
  rename_all(toupper) %>%
  pivot_longer(c("NO.RUMORS REPORTED - 10":"NO.SUSPECTS - 10"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month),  "Cumulative", month),
         source="MSR_Surv",
         sheet="hotline") %>%
  filter(COUNTY!="Totals")

# Animal Rumours Ensure no double counting
df_animal<- openxlsx::read.xlsx(df_filepath_animals, startRow=2, fillMergedCells = TRUE, sep.names=" ")
df_animal2<-df_animal %>%
  select(-"S/N", -"Animal Types - 10") %>%
  rename_all(toupper) %>%
  pivot_longer(c("NO.RUMORS REPORTED - 10":"NO.SUSPECTS - 10"), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>%
  mutate(month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month),  "Cumulative", month),
         source="MSR_Surv",
         sheet="animals") %>%
  filter(COUNTY!="Totals")

############################### SCRATCH

# df_msr_surv_1 %>% 
#   filter(indicator=="Pipe Filters Distributed This Month ",
#          month=="October",
#          COUNTY=="UROR") %>% 
#   group_by(PAYAM, `REPORTING UNIT`) %>% 
#   summarise(across(c(value), sum, na.rm = TRUE)) %>% 
#   View()



# df_msr_surv %>% 
#   group_by(COUNTY) %>% 
#   summarise(across(c(`Total Number of Rumours - 2021`,
#                      `Total number of rumours investigated - 2021`, 
#                      `Total number of rumours investigated within 24 hours - 2021`), sum, na.rm = TRUE)) %>%
#   mutate(investigated_percent= `Total number of rumours investigated within 24 hours - 2021`/`Total Number of Rumours - 2021`) %>% 
#   View()

