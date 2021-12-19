# PURPOSE: Munge and analyze Abate Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 15, 2021
# NOTES:

`%notin%` <- Negate(`%in%`)

df_filepath_abate <- "~/Github/dewormr/Data/Databases (November 2021)/2021 Abate database. Updated. 17.12.2021.LJ..xlsx"
df_abate<- read_xlsx(df_filepath_abate, sheet="2021 Abate Report", skip=1)

#Abate Data
df_abate_2 <- df_abate %>%
  filter(STATE!="NA") %>% 
  select("STATE", "COUNTY", "PAYAM", "BOMA", "SUPERVISORY AREA", "REPORTING UNIT", "REPORTING UNIT CODE",
         "NAME OF WATER SOURCE", "Combined/ Merged Water Source Name", "Water Source ID", "Type of Water Source",
         "Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using",
         contains("Targeted"), contains("Eligible"), contains("Treated")) %>% 
  mutate(across(c(contains("Y = 1")), as.character)) %>% 
  pivot_longer(c(contains("Y = 1"), contains("If not treated, why?")), names_to="indicator", values_to="value") %>%
  separate(indicator, c("indicator", "month"), sep="-") %>% 
  mutate(reason_no_abate=value,
         reason_no_abate=replace(reason_no_abate, reason_no_abate %notin% c("DRY", "NEGATIVE CDC RESULT", 
                                                                            "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA),
         value=replace(value, value %in% c("DRY", "NEGATIVE CDC RESULT", 
                                           "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA),
         value=as.numeric(value),
         month=as.numeric(month),
         month=month.name[month],
         month=if_else(is.na(month), "Cumulative", month),
         source="Abate",
         sheet="Abate")   
