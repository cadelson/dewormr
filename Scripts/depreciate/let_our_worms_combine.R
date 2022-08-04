# PURPOSE: Append data streams into one big ol' file
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 14, 2021
# NOTES: Feb 2022- Now writes file

data_out <- "~/Github/dewormr/Dataout"

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


rm(df_msr_cr, df_msr_cr_1, df_msr_surv, df_msr_surv_1, df_abate, df_abate_2, df_animal,
   df_animal2, df_hotline, df_hotline2, df_IDSR, df_IDSR2, df_non_msr_cr, df_non_msr_cr_1,
   df_non_msr_surv, df_non_msr_surv_1, df_RPIF, df_RPIF2, df_rough)

write_csv(df_21, file.path(data_out, "GWSD_2021_Final.txt"))
