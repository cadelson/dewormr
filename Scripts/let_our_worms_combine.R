# PURPOSE: Append data streams into one big ol' file
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 14, 2021
# NOTES:

df_rough<-bind_rows(df_msr_surv_1, df_non_msr_surv_1, df_RPIF2, df_IDSR2, df_hotline2,
              df_animal2, df_msr_cr_1, df_non_msr_cr_1, df_abate_2)

df<-df_rough %>% 
  clean_names() %>% remove_empty() %>% 
  mutate(across(c("state", "county", "payam", "boma", "supervisory_area",
                  "reporting_unit", "name_of_villages_combined", "name_of_water_source", 
                  "combined_merged_water_source_name", "water_source_id", "type_of_water_source",
                  "endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using", "reason_no_abate"), str_to_title)) %>%
  rename(vas=village_under_active_surveillance_1_yes_0_no,
         villages_in_reporting_unit=number_of_villages_per_reporting_unit,
         cc=is_this_village_a_cc_yes_1_0_no,
         cc_occupied=is_the_village_cc_occupied_not_occupied_yes_1_no_0,
         ev_using_water_source=endemic_villages_using_water_source_if_no_e_vs_then_1_villages_using) %>% 
  mutate(
    cc = case_when(
      str_detect(reporting_unit, "Cc") ~ "1",
      TRUE ~ "0"),
    state=case_when(
      state=="Wbg" ~"Western Bahr El Ghazal",
      state=="Lakes State" ~"Lakes",
      state=="Ees" ~ "Eastern Equatoria",
      TRUE ~ state),
    county=case_when(
      county=="Jur-River" ~ "Jur River",
      county=="Panyijar" ~ "Panyijiar",
      county=="Rumbek Center" ~ "Rumbek Centre",
      TRUE~county),
    indicator=tolower(indicator),
    indicator = case_when(
      indicator=="population " ~ "pop_total",
      indicator=="number of hhs " ~ "hh_total",
      indicator=="number of handpumps working " ~ "hp_working",
      indicator=="handpumps not working " ~ "hp_not_working",
      indicator=="number of protected wells " ~ "protected_wells",
      indicator=="hhs with cloth filters " ~ "filter_hh_cloth",
      indicator=="numb cloth filters distributed this month" ~ "filter_dist_cloth",
      indicator=="pipe filters distributed this month " ~ "filter_dist_pipe",
      indicator=="number of unsafe water source " ~ "abate_ws_unsafe",
      indicator=="unsafe water sources treated " ~ "abate_ws_treated",
      indicator=="invest rumours & following suspects " ~ "rumours_invest_suspects",
      indicator=="active gw case search " ~ "activity_case_search",
      indicator=="cash reward awareness " ~ "activity_cra",
      indicator=="community meeting " ~ "activity_community_meeting",
      indicator=="assessment " ~ "activity_assessment",
      indicator=="training " ~ "activity_training",
      indicator=="other " ~ "activity_other",
      indicator=="distribution of filters " ~ "filter_dist",
      indicator=="self reported " ~ "rumours_self",
      indicator=="informer reported " ~ "rumours_informer",
      indicator=="total number of rumours " ~ "rumours_total",
      indicator=="total number of rumours investigated " ~ "rumours_invest",
      indicator=="total number of rumours investigated within 24 hours " ~ "rumours_invest_24",
      indicator=="total number of suspects " ~ "suspects_total",
      indicator=="total number of suspects healed " ~ "suspects_healed",
      indicator=="number of new cases reported " ~ "cases_new",
      indicator=="number of new cases contained this month " ~ "cases_contained",
      indicator=="number of cases imported this month " ~ "cases_imported",
      indicator=="total number of training sessions conducted " ~ "training_total",
      indicator=="as/hw " ~ "visit_as_hw",
      indicator=="pso/fo " ~ "visit_pso_fo",
      indicator=="cso/po/spo/ta " ~ "visit_cso_po_spo_ta",
      indicator=="ssgwep secretariat " ~ "visit_secretariat",
      indicator=="expected " ~ "report_expected",
      indicator=="received " ~ "report_received",
      indicator=="numb cloth filters distributed this month " ~ "filter_dist_cloth",
      indicator=="number of ass" ~ "staff_as",
      indicator=="number of male ass" ~ "staff_as_male",
      indicator=="number of female ass" ~ "staff_as_female",
      indicator=="number of vvs" ~ "staff_vv",
      indicator=="number of vvs " ~ "staff_vv",
      indicator=="number of male vvs" ~ "staff_vv_male",
      indicator=="number of female vvs" ~ "staff_vv_female",
      indicator=="suspects" ~ "suspects_total",
      indicator=="no.rumours " ~ "rumours_total",
      indicator=="no.rumours investigated " ~ "rumours_invest",
      indicator=="no.rumours investigated within 24 hrs " ~ "rumours_invest_24",
      indicator=="suspects " ~ "suspects_total",
      indicator=="no.rumors reported " ~ "rumours_total",
      indicator=="no.rumors investigated " ~ "rumours_invest",
      indicator=="no.rumors invest.24 hrs " ~ "rumours_invest_24",
      indicator=="no.suspects " ~ "suspects_total",
      indicator=="total number of residents reached with cash reward messages " ~ "cr_reached",
      indicator=="total number of days on which cash reward activities were conducted " ~ "cr_days",
      indicator=="flipchart " ~ "ed_flipchart",
      indicator=="community event activity " ~ "ed_community_event",
      indicator=="video show " ~ "ed_video",
      indicator=="audio activity " ~ "ed_audio",
      indicator=="poster activity " ~ "ed_poster",
      indicator=="integrated activity " ~ "ed_integrated",
      indicator=="cso/po/cgwfp/  sfc/spo/ta " ~ "visit_cso_po_spo_ta",
      indicator=="cso/po/cgwfp/  sfc/spo/ta " ~ "visit_cso_po_spo_ta",
      indicator=="cso/ po/ cgwfp/ sfc/ spo/ ta " ~ "visit_cso_po_spo_ta",
      indicator=="abate_targeted" ~ "abate_targeted",
      indicator=="abate_eligible" ~ "abate_eligible",
      indicator=="abate_treated" ~ "abate_treated",
      indicator=="if not treated, why? " ~ "abate_reason_untreated"))

rm(df_msr_cr, df_msr_cr_1, df_msr_surv, df_msr_surv_1, df_abate, df_abate_2, df_animal,
   df_animal2, df_hotline, df_hotline2, df_IDSR, df_IDSR2, df_non_msr_cr, df_non_msr_cr_1,
   df_non_msr_surv, df_non_msr_surv_1, df_RPIF, df_RPIF2, df_rough)

df %>% 
  filter(month=="November",
         payam %in% c("Malueth", "Puluk", "Dor", "Paweng", 
                      "Mayen", "Makuac", "Wunlit"),
         indicator %in% c("staff_vv", "hp_working", "hh_total", "filter_hh_cloth", "pop_total",
                          "filter_dist_pipe", "abate_eligible", "abate_treated", "cr_day"))

df %>% 
  filter(month=="November",
         indicator %in% c("rumours_total", "rumours_invest", 
                              "rumours_invest_24", "suspects_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(county, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(month=="October",
         county=="Tonj South",
         indicator %in% c("rumours_total", "rumours_invest", 
                          "rumours_invest_24", "suspects_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(month=="October",
         indicator=="rumours_total") %>% 
  group_by(sheet) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(month=="November",
         indicator %in% c("rumours_total"),
         sheet %in% c("hotline", "IDSR")) %>% 
  group_by(state, county, payam, sheet, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

  
df %>% 
  filter(month=="November",
         indicator %in% c("rumours_self", "rumours_informer"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(month=="October",
         county=="Tonj South",
         indicator %in% c("rumours_self", "rumours_informer"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(month=="November",
         indicator %in% c("rumours_self", "rumours_informer"),
         sheet %in% c("RPIF")) %>% 
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(sheet=="RPIF",
         county=="Kapoeta East") %>% 
  View()
  
  
df %>% 
  filter(month=="November",
         reporting_unit %in% c("Tomrok", "Apukdit", "Block 1", "Kengel Cc"),
         indicator %in% c("protected_wells")) %>% 
  group_by(payam, boma, reporting_unit, sheet, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

                

df %>% 
  distinct(indicator) %>% 
  print(n=Inf)

df %>% 
  distinct(month) %>% 
  print(n=Inf)

df %>% distinct(county) %>% arrange(county) %>% print(n=Inf)

df_clean %>% 
  filter(payam %in% c("Paweng", "Pieri", "Malueth", "Puluk"),
         month=="October") 
  
df_clean %>% 
  filter(reporting_unit %in% c("Apukdit", "Wunethony", "Kengel Cc", "Tomrok"),
         month=="Cumulative",
         indicator %in% c("staff_vv", "hh_total", "pop_total", "hp_working")) %>%
  group_by(reporting_unit, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()


df %>% 
  filter(month=="October",
         indicator %in% c("abate_targeted", "abate_eligible", "abate_treated"),
         county=="Tonj East") %>%
  group_by(state, county, payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

  

  df_clean %>% 
    filter(reason_no_abate!="NA") %>%
    group_by(month, state, county, payam, reason_no_abate) %>% 
    count(reason_no_abate, payam, sort=TRUE) %>% 
    View() m

#   group_by(COUNTY) %>% 
#   summarise(across(c(`Total Number of Rumours - 2021`,
#                      `Total number of rumours investigated - 2021`, 
#                      `Total number of rumours investigated within 24 hours - 2021`), sum, na.rm = TRUE)) %>%
#   mutate(investigated_percent= `Total number of rumours investigated within 24 hours - 2021`/`Total Number of Rumours - 2021`) %>% 
#   View()
