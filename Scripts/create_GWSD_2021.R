# PURPOSE: Munge and analyze 2021 MSR-Surv, Cash Reward and Abate Databases for routine analyses and append
# AUTHOR: Cody Adelson
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: April 27, 2022
# NOTES: Consolidated from separate files for each database

`%notin%` <- Negate(`%in%`)
data_out <- "~/Github/dewormr/Dataout"
df_filepath_surv_21 <- "~/Github/dewormr/Data/2021_raw_data/Databases (Final 2021)/2021 MSR-Surv database Updated .11.2.2022.Final _LJ^.xlsx"
df_filepath_hotline_21 <- "~/Github/dewormr/Data/2021_raw_data/Databases (Final 2021)/Hotline report Final -2021 Stitched.xlsx"
df_filepath_animals_21 <- "~/Github/dewormr/Data/2021_raw_data/Databases (Final 2021)/Animal Rumors 2021_Final^J2021.LJ_STITCHED.xlsx"
df_filepath_cr_21 <- "~/Github/dewormr/Data/2021_raw_data/Databases (Final 2021)/2021 MSR-CR Database _Final. 15-02-2022.LJ^.xlsx"
df_filepath_abate_21 <- "~/Github/dewormr/Data/2021_raw_data/Databases (Final 2021)/2021 Abate database. Updated. 16.2.2021..xlsx"

#============== MSR_SURV database
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
         sheet="Non_MSR_Other") %>% 
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

#==============Cash Reward database
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

#============== Abate database
df_abate<- read_xlsx(df_filepath_abate_21, sheet="2021 Abate Report", skip=1)
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

#============== Combine databases into one df
df_21_rough<-bind_rows(df_msr_surv, df_non_msr_surv, df_RPIF, df_IDSR, df_hotline,
                       df_animal, df_msr_cr, df_non_msr_cr, df_abate)

write_csv(df_21_rough, file.path(data_out, "GWSD_2021_ROUGH.txt"))

df_21<-df_21_rough %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(across(c("state", "county", "payam", "boma", "supervisory_area",
                  "reporting_unit", "name_of_water_source", 
                  "type_of_water_source",
                  "endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using", "reason_no_abate"), str_to_title)) %>%
  rename(vas=village_under_active_surveillance_1_yes_0_no,
         ev_using_water_source=endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using) %>% 
  mutate(
    cc = case_when(
      str_detect(reporting_unit, "Cc") ~ "1",
      TRUE ~ "0"),
    state=case_when(
      state=="Wbg" ~"Western Bahr El Ghazal",
      state=="Lakes State" ~"Lakes",
      state=="Ees" ~ "Eastern Equatoria",
      county=="Abeyi" ~ "Abeyi Admin. Area",
      county=="Duk" ~ "Jonglei",
      county=="Fashoda" ~ "Upper Nile",
      county=="Juba" ~ "Central Equatoria",
      county %in% c("Tonj East", "Tonj North", "Gogrial West") ~ "Warrap",
      county %in% c("Uror") ~ "Jonglei",
      county %in% c("Magwei", "Magwi", "Kapoeta East", "Kapoeta North", "Kapoeta South", "Ikotos",
                    "Lopa/Lafon", "Torit") ~ "Eastern Equatoria",
      county %in% c("Panyijiar", "Panyijar", "Rukbkona", "Rubkona") ~ "Unity",
      county %in% c("Awerial", "Mayom", "Rumbek East", "Terekeka", "Yirol West", "Rumbek Centre", "Rumbek Center",
                    "Rumbek North", "Wulu") ~ "Lakes",
      county %in% c("Aweil West", "Aweil Center") ~ "Northern Bahr El Ghazal",
      county %in% c("Jur River", "Jur-River", "Raja", "Wau") ~ "Western Bahr El Ghazal",
      payam %in% c("Longeleya", "Machi I", "Machi II", "Pwata", "Machi Ii") ~ "Eastern Equatoria",
      TRUE ~ state),
    county=case_when(
      county=="Jur-River" ~ "Jur River",
      county=="Machi Ii" ~ "Machi II",
      county=="Panyijar" ~ "Panyijiar",
      county=="Rumbek Center" ~ "Rumbek Centre",
      county=="Rukbkona" ~ "Rubkona",
      county=="Magwei" ~ "Magwi",
      payam=="Longeleya" ~ "Kapoeta South",
      payam %in% c("Machi I", "Machi II", "Pwata", "Machi Ii") ~"Kapoeta South",
      TRUE~county),
    payam=case_when(
      payam=="Pulchol" ~"Pulchuol",
      payam=="Nile" & reporting_unit=="Panakech"~"Dor",
      TRUE~payam),
    reporting_unit=case_when(
      reporting_unit=="Wechkotda"|reporting_unit=="Wechkoda"|reporting_unit=="Wech Kotda"|reporting_unit=="Wech Koteda"|reporting_unit=="Wechkoida" ~ "Wechotda",
      reporting_unit %in% c("Manyak", "Nyakhar Manyak") ~"Nyakhor Manyak",
      reporting_unit=="Wun Thony"~"Wunethony",
      reporting_unit=="Nyakhor  Kamel"~"Nyakhor Kamel",
      TRUE~reporting_unit),
    reporting_unit=str_replace(reporting_unit, "Cc", "CC"),
    indicator=tolower(indicator),
    indicator = case_when(
      indicator=="population" ~ "pop_total",
      indicator=="number of hhs" ~ "hh_total",
      indicator=="number of handpumps working" ~ "hp_working",
      indicator=="handpumps not working" ~ "hp_not_working",
      indicator=="number of protected wells" ~ "protected_wells",
      indicator=="hhs with cloth filters" ~ "filter_hh_cloth",
      indicator=="numb cloth filters distributed this month" ~ "filter_dist_cloth",
      indicator=="pipe filters distributed this month" ~ "filter_dist_pipe",
      indicator=="number of unsafe water source" ~ "abate_ws_unsafe",
      indicator=="unsafe water sources treated" ~ "abate_ws_treated",
      indicator=="invest rumours & following suspects" ~ "rumours_invest_suspects",
      indicator=="active gw case search" ~ "activity_case_search",
      indicator=="cash reward awareness" ~ "activity_cra",
      indicator=="community meeting" ~ "activity_community_meeting",
      indicator=="assessment" ~ "activity_assessment",
      indicator=="training" ~ "activity_training",
      indicator=="other" ~ "activity_other",
      indicator=="distribution of filters" ~ "filter_dist",
      indicator=="self reported" ~ "rumours_self",
      indicator=="informer reported" ~ "rumours_informer",
      indicator=="total number of rumours" ~ "rumours_total",
      indicator=="total number of rumours investigated" ~ "rumours_invest",
      indicator=="total number of rumours investigated within 24 hours" ~ "rumours_invest_24",
      indicator=="total number of suspects" ~ "suspects_total",
      indicator=="total number of suspects healed" ~ "suspects_healed",
      indicator=="number of new cases reported" ~ "cases_new",
      indicator=="number of new cases contained this month" ~ "cases_contained",
      indicator=="number of cases imported this month" ~ "cases_imported",
      indicator=="total number of training sessions conducted" ~ "training_total",
      indicator=="as/hw" ~ "visit_as_hw",
      indicator=="pso/fo" ~ "visit_pso_fo",
      indicator=="cso/po/spo/ta" ~ "visit_cso_po_spo_ta",
      indicator=="ssgwep secretariat" ~ "visit_secretariat",
      indicator=="expected" ~ "report_expected",
      indicator=="received" ~ "report_received",
      indicator=="numb cloth filters distributed this month" ~ "filter_dist_cloth",
      indicator=="number of ass" ~ "staff_as",
      indicator=="number of male ass" ~ "staff_as_male",
      indicator=="number of female ass" ~ "staff_as_female",
      indicator=="number of vvs" ~ "staff_vv",
      indicator=="number of vvs" ~ "staff_vv",
      indicator=="number of male vvs" ~ "staff_vv_male",
      indicator=="number of female vvs" ~ "staff_vv_female",
      indicator=="suspects" ~ "suspects_total",
      indicator=="no.rumours" ~ "rumours_total",
      indicator=="no.rumours investigated" ~ "rumours_invest",
      indicator=="no.rumours investigated within 24 hrs" ~ "rumours_invest_24",
      indicator=="suspects" ~ "suspects_total",
      indicator=="no.rumors reported" ~ "rumours_total",
      indicator=="no.rumors investigated" ~ "rumours_invest",
      indicator=="no.rumors invest.24 hrs" ~ "rumours_invest_24",
      indicator=="no.suspects" ~ "suspects_total",
      indicator=="total number of residents reached with cash reward messages" ~ "cr_reached",
      indicator=="total number of days on which cash reward activities were conducted" ~ "cr_days",
      indicator=="flipchart" ~ "ed_flipchart",
      indicator=="community event activity" ~ "ed_community_event",
      indicator=="video show" ~ "ed_video",
      indicator=="audio activity" ~ "ed_audio",
      indicator=="poster activity" ~ "ed_poster",
      indicator=="integrated activity" ~ "ed_integrated",
      indicator=="cso/po/cgwfp/  sfc/spo/ta" ~ "visit_cso_po_spo_ta",
      indicator=="cso/po/cgwfp/  sfc/spo/ta" ~ "visit_cso_po_spo_ta",
      indicator=="cso/ po/ cgwfp/ sfc/ spo/ ta" ~ "visit_cso_po_spo_ta",
      indicator=="if not treated, why?" ~ "abate_reason_untreated",
      TRUE ~ indicator))


rm(df_msr_cr, df_msr_surv, df_abate, df_animal, df_hotline, df_IDSR, df_non_msr_cr,
   df_non_msr_surv, df_RPIF, df_rough)

write_csv(df_21, file.path(data_out, "GWSD_2021_Final.txt"))
