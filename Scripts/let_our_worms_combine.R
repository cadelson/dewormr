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
      county=="Abeyi" ~ "Abeyi Admin. Area",
      county=="Awerial" ~ "Lakes",
      county=="Duk" ~ "Jonglei",
      county=="Fashoda" ~ "Upper Nile",
      county=="Juba" ~ "Central Equatoria",
      county=="Kapoeta East" ~ "Eastern Equatoria",
      county=="Kapoeta North" ~ "Eastern Equatoria",
      county=="Kapoeta South" ~ "Eastern Equatoria",
      county=="Mayom" ~ "Lakes",
      county=="Panyijiar" ~ "Unity",
      county %in% c("Rukbkona", "Rubkona") ~  "Unity",
      county=="Rumbek East" ~ "Lakes",
      county=="Terekeka" ~ "Lakes",
      county=="Yirol West" ~ "Lakes",
      payam=="Longeleya" ~ "Eastern Equatoria",
      payam=="Machi I" ~"Eastern Equatoria",
      payam=="Machi II" ~"Eastern Equatoria",
      payam=="Pwata" ~ "Eastern Equatoria",
      TRUE ~ state),
    county=case_when(
      county=="Jur-River" ~ "Jur River",
      county=="Panyijar" ~ "Panyijiar",
      county=="Rumbek Center" ~ "Rumbek Centre",
      county=="Rukbkona" ~ "Rubkona",
      payam=="Longeleya" ~ "Kapoeta South",
      payam=="Machi I" ~"Kapoeta South",
      payam=="Machi II" ~"Kapoeta South",
      payam=="Pwata" ~ "Kapoeta South",
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
      reporting_unit %in% c("Rumdit 1", "Rumdit 2") ~ "Rumdit",
      TRUE~reporting_unit),
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
      indicator=="if not treated, why? " ~ "abate_reason_untreated")) %>% 
  mutate("value"=case_when(
    indicator=="report_expected" & county %in% c("Yirol West", "Rumbek East", "Yirol East", "Aweil Centre", "Raja", "Mvolo") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 0,
    indicator=="report_expected" & county %in% c("Akobo", "Gogrial West", "Ikotos", "Jur River",
                                                 "Lopa/Lafon", "Nyirol", "Rumbek Centre", "Rumbek North",
                                                 "Tonj East", "Tonj South", "Torit", "Uror", "Wau", "Wulu") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 1,
    indicator=="report_expected" & county=="Awerial" & payam %in% c("Dor", "Puluk") & month %in% c("November", "December") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 1,
    indicator=="report_expected" & county=="Awerial" & payam %notin% c("Dor", "Puluk") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 0,
    indicator=="report_expected" & county=="Awerial" & payam %in% c("Dor", "Puluk") & month %notin% c("November", "December") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 0,
    indicator=="report_expected" & county=="Tonj North" & payam %in% c("Akop", "Marial-Lou") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 1,
    indicator=="report_expected" & county=="Tonj North" & payam %in% c("Aliek", "Awuul", "Kirik", "Man-Loor", "Pagol", "Rual-Bet") & sheet %in% c("MSR_Surv", "Non_MSR_Surv") ~ 0,
    TRUE~value))

df %>% 
  filter(county %in% c("Awerial", "Tonj North"),
         payam %in% c("Dor", "Puluk", "Akop", "Marial-Lou"))


rm(df_msr_cr, df_msr_cr_1, df_msr_surv, df_msr_surv_1, df_abate, df_abate_2, df_animal,
   df_animal2, df_hotline, df_hotline2, df_IDSR, df_IDSR2, df_non_msr_cr, df_non_msr_cr_1,
   df_non_msr_surv, df_non_msr_surv_1, df_RPIF, df_RPIF2, df_rough)

#Check reports received
df %>%
  filter(indicator %in% c("report_expected", "report_received"),
         month %notin% c("Cumulative", "December"),
         reporting_unit!="Wech Nyhal",
         vas=="1") %>%
  select(state, county, payam, boma, reporting_unit, indicator, month, value, sheet) %>%
  mutate(row=row_number()) %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  select(-row) %>%
  group_by(state, county, payam, month, reporting_unit, sheet) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  filter(report_expected>report_received) %>%
  write_xlsx(file.path(data_out, "non_reporting_reporting_unit_vas.xlsx"))

df %>%
  filter(indicator %in% c("report_expected", "report_received"),
         month %notin% c("Cumulative", "December"),
         reporting_unit!="Wech Nyhal") %>%
  select(state, county, payam, boma, reporting_unit, indicator, month, value, sheet, vas) %>%
  mutate(row=row_number()) %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  select(-row) %>%
  group_by(vas, state, county, payam, reporting_unit, month, sheet) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  filter(report_expected>report_received) %>%
  write_xlsx(file.path(data_out, "non_reporting_reporting_unit.xlsx"))

df %>% 
  filter(indicator=="abate_targeted",
         month=="November",
         payam=="Makuac") %>% 
  group_by(payam, indicator, month) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  View()

df %>% 
  filter(source=="MSR_Surv",
         payam=="Paweng",
         month !="December",
         month!="Cumulative") %>% 
  group_by(payam, month) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  View()


df %>% 
  filter(county %in% c("Tonj East"),
         month %in% c("November"),
         indicator %in% c("rumours_total", "rumours_invest", "rumours_invest_24",
                          "suspects_total")) %>% 
  group_by(payam, month, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  print(n=Inf)

df %>% 
  filter(payam=="Tonj",
         month=="August",
         indicator %in% c("abate_targeted", "abate_eligible", "abate_treated")) %>% 
  group_by(reporting_unit, month, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()
  
df %>% 
  filter(county %in%  c("Tonj East"),
         month %in% c("November"),
         indicator %in% c("rumours_total", "rumours_self", "rumours_informer")) %>% 
  group_by(payam, month, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  print(n=Inf)

df %>% 
  filter(county=="Wau",
         month=="January",
         indicator %in% c("rumours_total")) %>%
  group_by(county, month, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  print(n=Inf)

df %>% 
  filter( month=="January",
         indicator %in% c("rumours_total")) %>%
  group_by(month, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  print(n=Inf)

df %>% 
  filter(reporting_unit=="Achol Manak",
         month=="August") %>% 
  group_by(county, payam, month, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(vas=="1",
         county=="Tonj East",
         indicator %in% c("report_expected", "report_received"),
         month=="November") %>% 
  group_by(payam, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))
