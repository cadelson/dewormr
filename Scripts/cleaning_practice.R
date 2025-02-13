# PURPOSE: Practice cleaning in R and data visualization with ggplot
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: November 1, 2024
# NOTES:

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(scales)
library(extrafont)
library(writexl)
library(glitr)


cleaning_sample_raw <- read_excel("Data/cleaning_sample_data.xlsx", sheet = "raw_data")
data_out <- "~/Github/dewormr/Dataout"


# Examine your dataset

# cleaning_sample_raw %>%
#   glimpse()
# 
# cleaning_sample_raw %>%
#   View()

#### Putting it all together
cleaning_sample_clean <- cleaning_sample_raw %>% 
  # Convert variable names to snake case
  clean_names() %>% 
  # Rename variables
  rename(reporting_unit = name_of_report_ing_unit,
         county = county_name,
         latitude = lat,
         longitude = long) %>% 
  # Remove unused variable
  select(-x35) %>% 
  # Convert character variables to title case, fix misspellings, and 
  # fix non-numeric characters in numeric variables
  mutate(state = str_to_title(state),
         county = str_to_title(county),
         reporting_unit = str_to_title(reporting_unit),
         state = str_replace_all(state, "Warrab", "Warrap"),
         state = str_replace_all(state, "Lakse", "Lakes"),
         county = case_when(
           county == "Torait" ~ "Torit",
           county == "Rumbek Center" ~ "Rumbek Centre",
           county == "Tonj E." ~ "Tonj East",
           TRUE ~ county),
         hh = str_replace_all(hh, "o", "0"),
         hh = as.numeric(hh),
         rumours_6 = str_replace_all(rumours_6, "O", "0"),
         rumours_6 = as.numeric(rumours_6)) %>% 
  # Remove the total from the dataset
  filter(state != "Total")

write_xlsx(cleaning_sample_clean, file.path(data_out, "sample_data_output.xlsx"))


# Now let's put it together for visualization 

month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

cleaning_sample_viz <- cleaning_sample_clean %>% 
  select(county, contains("rumours"), contains("filters")) %>% 
  pivot_longer(where(is.numeric), names_to="indicator", values_to="value") %>% 
  mutate(indicator = str_replace_all(indicator, "filters_distributed", "filters")) %>% 
  separate(indicator, c("indicator", "month"), sep="_") %>%
  summarise(rumours = sum(value), .by = c(month, indicator)) %>% 
  mutate(month_abb = month.abb[as.numeric(month)],
         month_abb = fct_relevel(month_abb, month_order))

cleaning_sample_viz %>% 
  ggplot(aes(x = month_abb, y = rumours))+
  geom_bar(stat = "identity", fill = genoa) +
  scale_y_continuous(labels = comma, breaks = seq(0, 20000, 5000), limits = c(0,20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL,
       title = "Program Monthly Rumours",
       subtitle = "GWEP reported rumours have remained stable throughout 2024")+
  theme(axis.text.x  = element_text(vjust = 4, family = "Source Sans Pro", size = 14),
        axis.text.y  = element_text(family = "Source Sans Pro", size = 14),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  #scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})+
  scale_fill_identity()+
  scale_color_identity() +
  geom_text(aes(label = comma(rumours)), size = 4, vjust = -.3, family = "Source Sans Pro") 

si_save("Images/cleaning_sample_rumours")  


cleaning_sample_rumours <- cleaning_sample_clean %>% 
  select(state, county, reporting_unit, contains("rumours"))

cleaning_sample_filters <- cleaning_sample_clean %>% 
  select(state, county, reporting_unit, contains("filter"))


cleaning_sample_join <- cleaning_sample_rumours %>% 
  left_join(cleaning_sample_filters)

cleaning_sample_join_torit <- cleaning_sample_join %>% 
  filter(county == "Torit")

cleaning_sample_join_te <- cleaning_sample_join %>% 
  filter(county == "Tonj East")

cleaning_sample_bind <- cleaning_sample_join_torit %>% 
  bind_rows(cleaning_sample_join_te)

current_month <- "July"

df_22_24 %>% 
  filter(year %in% c("2024"),
         indicator %in% c("report_expected", "report_received"),
         month == current_month
         ) %>% 
  select(county, indicator, month, value, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(county, indicator, month)) %>% 
  View()

df_22_24 %>% 
  filter(year %in% c("2024"),
         indicator %in% c("report_received"),
         month == current_month
  ) %>% 
  select(county, indicator, month, value, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(county, indicator, month)) %>% 
  View()
df_22_24 %>% 
  filter(year %in% c("2024"),
         indicator %in% c("report_expected"),
         month == current_month
  ) %>% 
  select(county, indicator, month, value, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(county, indicator, month)) %>% 
  View()
df_22_24 %>% 
  filter(year %in% c("2024"),
         indicator %in% c("suspects_total"),
         month == current_month
  ) %>% 
  select(county, indicator, month, value, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(county, indicator, month)) %>% 
  View()

new_dataset_name <- df_22_24 %>% 
  filter(year %in% c("2024"),
         indicator %in% c("rumours_total"),
         month == "July") %>% 
  select(county, indicator, month, value, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(county, indicator, month))
