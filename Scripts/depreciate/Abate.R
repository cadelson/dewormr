# PURPOSE: Munge and analyze Abate Database for routine analyses and prepare for appending with other program data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 15, 2021
# NOTES:

`%notin%` <- Negate(`%in%`)

df_filepath_abate_21 <- "~/Github/dewormr/Data/Databases (Final 2021)/2021 Abate database. Updated. 16.2.2021..xlsx"
df_abate<- read_xlsx(df_filepath_abate_21, sheet="2021 Abate Report", skip=1)

  #Abate Data
  df_abate <- df_abate %>%
    filter(STATE!="NA") %>%
    select(STATE:`Type of Water Source`, `Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using`,
           contains("Targeted"), contains("Eligible"), contains("Treated"), contains("Amount"), -`Volume of Water Source (m3)`, -`Amount of Abate Added (mL)`) %>% 
    mutate(across(c(contains("Y = 1"), contains("Amount")), as.character)) %>% 
    pivot_longer(c(contains("Y = 1"), contains("If not treated, why?"), contains("Amount")), names_to="indicator", values_to="value") %>%
    mutate(indicator=gsub("\\(P.*", "", indicator)) %>%
    separate(indicator, c("indicator", "month"), sep="-") %>%
    mutate(reason_no_abate=value,
           reason_no_abate=replace(reason_no_abate, reason_no_abate %notin% c("DRY", "NEGATIVE CDC RESULT", 
                                                                              "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA),
           value=as.numeric(replace(value, value %in% c("DRY", "NEGATIVE CDC RESULT", 
                                                        "OTHER", "FLOODED", "FLOWING", "INSECURITY", "dry"), NA)),
           month=month.name[as.numeric(month)],
           source="Abate",
           sheet="Abate") %>% 
    mutate(row = row_number(),
           indicator=fct_relabel(indicator, trimws)) %>% 
    pivot_wider(names_from=indicator, values_from = value) %>% 
    select(-row) %>% 
    group_by(STATE, COUNTY, PAYAM, BOMA, `SUPERVISORY AREA`, `REPORTING UNIT`, `REPORTING UNIT CODE`,
             `NAME OF WATER SOURCE`, `Combined/ Merged Water Source Name`, `Water Source ID`, 
             `Type of Water Source`, `Endemic Villages Using Water Source/ If no EV's then 1+ Villages Using`,
             month, reason_no_abate, source, sheet) %>% 
    #Causes loss of eligible sites that are duplicated. Ensure water source names are numbered to resolve issue
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    #Note March 9, 2022- removed to allow for double counting error in reporting
    # mutate(across(where(is.numeric), ~ case_when(
    #   .==2~1,
    #   TRUE ~ .))) %>%
    ungroup() %>% 
    mutate(
      "abate_targeted"=`Targeted\r\n(Y = 1, N = 0)`,
      "abate_eligible"=`Eligible\r\n(Y = 1, N = 0)`,
      "abate_treated"=`Treated\r\n(Y = 1, N = 0)`,
      "abate_used"=`Amount of Abate Added (mL)`,
      # Note March 9 2022- Removed because of double counting of water source. Check to make sure all eligible and treated are targeted
      # "abate_targeted"=case_when(
      #   abate_eligible=="1"|abate_treated=="1" ~ "1",
      #   TRUE ~ as.character(abate_targeted)),
      "abate_eligible"=case_when(
        abate_treated=="1" ~ "1",
        abate_treated=="0" ~ "0",
        TRUE ~ as.character(abate_eligible)),
      "abate_targeted"=as.double(abate_targeted),
      "abate_eligible"=as.double(abate_eligible),
      "abate_treated"=as.double(abate_treated)) %>% 
    select(-`Targeted\r\n(Y = 1, N = 0)`, -`Eligible\r\n(Y = 1, N = 0)`, -`Treated\r\n(Y = 1, N = 0)`, -`Amount of Abate Added (mL)`) %>% 
    pivot_longer(c(contains("abate_"), contains("If not treated, why?")), names_to="indicator", values_to="value")

  

  