library(tidyverse)
library(reshape2)
library(lubridate)
library(zoo)

# State name mappings
state_mapping <- as_tibble(data.frame(state.abb, state.name, state.region)) %>%
  rename(c("state_abbrev" = "state.abb", "state_name" = "state.name", "state_region" = "state.region")) %>%
  mutate(state_abbrev = as.character(state_abbrev), state_name = as.character(state_name))

# Data output directory
source("config.R")

# Pivot cols
pivot_cols <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
                "Country_Region", "Lat", "Long_", "Combined_Key")

# Confirmed cases
df_us_confirmed_long <- read_csv(paste(jhu_data_dir, 'time_series_covid19_confirmed_US.csv', sep="/")) %>% 
  pivot_longer(-all_of(pivot_cols), names_to = "report_date", values_to = "Confirmed") %>%
  mutate(report_date = parse_date(report_date, "%m/%d/%y")) %>%
  group_by(Province_State, report_date) %>%
  arrange(Province_State, report_date) %>%
  summarise(Cumulative_Confirmed = sum(Confirmed)) %>%
  mutate(Cumulative_Confirmed_MA = rollapply(Cumulative_Confirmed, 7, mean, align='right', fill=NA)) %>%    
  mutate(New_Confirmed = Cumulative_Confirmed - lag(Cumulative_Confirmed)) %>%
  arrange(Province_State, report_date) %>%    
  mutate(New_Confirmed_MA = rollapply(New_Confirmed, 7, mean, align='right', fill=NA))
    
# Confirmed deaths
df_us_deaths_long <- read_csv(paste(jhu_data_dir, 'time_series_covid19_deaths_US.csv', sep="/")) %>% 
  select(-Population) %>%
  pivot_longer(-all_of(pivot_cols), names_to = "report_date", values_to = "Deaths") %>%
  mutate(report_date = parse_date(report_date, "%m/%d/%y")) %>%
  group_by(Province_State, report_date) %>%
  summarise(Cumulative_Deaths = sum(Deaths)) %>%
  mutate(Cumulative_Deaths_MA = rollapply(Cumulative_Deaths, 7, mean, align='right', fill=NA)) %>%
  mutate(New_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths)) %>%
  mutate(New_Deaths_MA = rollapply(New_Deaths, 7, mean, align='right', fill=NA))

# Population from deaths file
df_us_population <- read_csv(paste(jhu_data_dir, 'time_series_covid19_deaths_US.csv', sep="/")) %>% 
  select(c(all_of(pivot_cols), Population)) %>%
  group_by(Province_State) %>%
  summarise(Population = sum(Population))
  
# us Confirmed / Dead 
df_us <- df_us_confirmed_long %>%
  left_join(df_us_deaths_long, by = c("Province_State", "report_date")) %>%
  left_join(df_us_population,  by = c("Province_State")) %>%
  mutate(New_Confirmed = Cumulative_Confirmed - lag(Cumulative_Confirmed),
         New_Deaths    = Cumulative_Deaths - lag(Cumulative_Deaths),
         New_Deaths_Per_Capita = New_Deaths / Population,
         New_Deaths_MA_Per_Capita = New_Deaths_MA / Population,         
         New_Confirmed_Per_Capita = New_Confirmed /  Population,
         New_Confirmed_MA_Per_Capita = New_Confirmed_MA /  Population,         
         Cumulative_Confirmed_Per_Capita = Cumulative_Confirmed /  Population,
         Cumulative_Deaths_Per_Capita = Cumulative_Deaths / Population,
         Cumulative_Confirmed_MA_Per_Capita = Cumulative_Confirmed_MA /  Population,
         Cumulative_Deaths_MA_Per_Capita = Cumulative_Deaths_MA / Population,
         Death_Rate = Cumulative_Deaths /  Cumulative_Confirmed) %>%
  arrange(Province_State, report_date) %>%    
  mutate(Death_Rate_MA = rollapply(Death_Rate, 7, mean, align='right', fill=NA))
  

# Export
write_csv(df_us, paste(app_data_dir, "us_cases.csv", sep="/"))
