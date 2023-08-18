library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(stringi)
library(writexl)

wd <- here()
raw_data_dir <- str_c(wd, '/Raw Data/')

# Finalize dataset.

counties <- read_xlsx(str_c(raw_data_dir, 'Cali Counties.xlsx')) %>%
  clean_names()
  
counties$county <- str_remove_all(counties$county, ' County')

county_year <- counties %>%
  mutate(year = 2010)

for (i in 2011:2020) {
  temp <- counties %>%
    mutate(year = i)
  
  county_year <- county_year %>%
    rbind(temp)
}

data <- tibble()

# Emission Data. ####

emissions2010 <- read_csv(str_c(raw_data_dir, '2010/Emissions 2010.csv'),
                          skip = 4) %>%
  clean_names() %>%
  select(-state)

crosswalk2010 <- read_csv(str_c(raw_data_dir, '2010/Crosswalk 2010.csv'), 
                          skip = 4) %>%
  clean_names() %>%
  select(plant_name, plant_code)

data <- 
  read_xls(str_c(raw_data_dir, '2010/PlantY2010.xls')) %>%
  clean_names() %>%
  filter(state == 'CA') %>%
  select(-plant_name) %>%
  left_join(crosswalk2010, by = 'plant_code') %>%
  select(plant_code, plant_name, county, state) %>%
  left_join(emissions2010, by = 'plant_name') %>%
  select(plant_code, plant_name, county, state, co2_emissions_metric_tons) %>%
  mutate(co2_emissions_metric_tons = as.numeric(co2_emissions_metric_tons)) %>%
  group_by(county = stri_trans_totitle(county)) %>%
  summarise(emissions = sum(co2_emissions_metric_tons, na.rm = T)) %>%
  ungroup() %>%
  right_join(counties) %>%
  mutate(year = 2010)

years <- c(2011:2020)

for (i in years) {
  emissions <- read_csv(str_c(raw_data_dir, i, '/Emissions ', i, '.csv'),
                        skip = 4) %>%
    clean_names() %>%
    select(-state)
    
  crosswalk <- read_csv(str_c(raw_data_dir, i, '/Crosswalk ', i, '.csv'), 
                        skip = 4) %>%
    clean_names() %>%
    select(plant_name, plant_code)
    
  primary_data <- 
    read_xlsx(str_c(raw_data_dir, i, '/PlantY', i, '.xlsx'), skip = 1) %>%
    clean_names() %>%
    filter(state == 'CA') %>%
    select(-plant_name) %>%
    left_join(crosswalk, by = 'plant_code') %>%
    select(plant_code, plant_name, county, state) %>%
    left_join(emissions, by = 'plant_name') %>%
    select(plant_code, plant_name, county, state, co2_emissions_metric_tons) %>%
    mutate(co2_emissions_metric_tons = as.numeric(co2_emissions_metric_tons)) %>%
    group_by(county = stri_trans_totitle(county)) %>%
    summarise(emissions = sum(co2_emissions_metric_tons, na.rm = T)) %>%
    ungroup() %>%
    right_join(counties) %>%
    mutate(year = i) 
  
  data <- data %>%
    rbind(primary_data)
}

# FiT Data. #####

fit <- counties %>%
  mutate(year = 2010)

for (i in 2011:2020) {
  temp <- counties %>%
    mutate(year = i)
  
  fit <- fit %>%
    rbind(temp)
}

fit <- fit %>%
  mutate(tariff = case_when(
    county == 'Santa Clara' & year >= 2012 ~ 1,
    county == 'Marin' & year >= 2012 ~ 1,
    county == 'Napa' & year >= 2012 ~ 1,
    county == 'Solano' & year >= 2012 ~ 1,
    county == 'Contra Costa' & year >= 2012 ~ 1,
    county == 'Humboldt' & year >= 2019 ~ 1,
    county == 'Sonoma' & year %in% 2014:2018 ~ 1,
    county == 'Mendocino' & year %in% 2014:2018 ~ 1,
    county == 'Sacramento' & year %in% 2010:2012 ~ 1,
    county == 'Los Angeles' & year >= 2013 ~ 1,
    TRUE ~ 0
  ))
# Population Data. ####

population <- read_csv(str_c(raw_data_dir, 'co-est2020-alldata.csv')) %>%
  filter(STNAME == 'California') %>%
  rename('county' = CTYNAME) %>%
  select(county, CENSUS2010POP, contains('POPESTIMATE')) %>%
  mutate(CENSUS2010POP = as.numeric(CENSUS2010POP)) %>%
  select(-POPESTIMATE2010) %>%
  pivot_longer(cols = -county, names_to = 'year', values_to = 'pop')

population$county <- str_remove_all(population$county, ' County')
population$year <- str_remove_all(population$year, 'ESTIMATE')
population$year <- str_remove_all(population$year, 'CENSUS')
population$year <- str_remove_all(population$year, 'POP')

population <- population %>%
  mutate(year = as.numeric(year)) %>%
  filter(county != 'California')

# GDP Data. ####

gdp <- read_csv(str_c(raw_data_dir, 'CAGDP1_CA_2001_2020.csv')) %>%
  rename('county' = GeoName) %>%
  filter(county != 'California' & 
           Description == 'Real GDP (thousands of chained 2012 dollars)')

gdp$county = str_remove_all(gdp$county, ', CA')

gdp <- gdp %>%
  select(county, contains('20')) %>%
  pivot_longer(cols = c(contains('20')), 
               names_to = 'year', values_to = 'real_gdp') %>%
  filter(year >= 2010) %>%
  mutate(year = as.numeric(year))

# Income Data. ####

income <- read_csv(str_c(raw_data_dir, 'CAINC1_CA_1969_2020.csv')) %>%
  rename('county' = GeoName) %>%
  filter(county != 'California' & 
           Description == 'Per capita personal income (dollars) 2/')

income$county = str_remove_all(income$county, ', CA')

income <- income %>%
  select(county, contains('20')) %>%
  pivot_longer(cols = c(contains('20')), 
               names_to = 'year', values_to = 'pc_income') %>%
  filter(year >= 2010) %>%
  mutate(year = as.numeric(year))
  
# Generation Capacity Data. ####

capacity <- read_xlsx(str_c(raw_data_dir, 'Solar Generation Capacity.xlsx')) %>%
  clean_names() %>%
  right_join(county_year)

# Electricity Consumption. ####

consumption <- read_csv(str_c(raw_data_dir, 'elecbycounty.aspx.csv')) %>%
  select(-`Total Usage`) %>%
  pivot_longer(cols = -c(`County`, `Sector`), 
               names_to = 'year', values_to = 'consumption') %>%
  pivot_wider(names_from = Sector, values_from = consumption) %>%
  clean_names() %>%
  filter(year >= 2010) %>%
  rename('non_residential_consumption' = non_residential) %>%
  rename('residential_consumption' = residential) %>%
  rename('total_consumption' = total) %>%
  mutate(year = as.numeric(year))
  
consumption$county = str_to_title(consumption$county)

# Racial Breakdown Data. ####

race_dir <- str_c(raw_data_dir, 'Racial Breakdown/')

race_percentages <- tibble()

for (i in 2010:2016) {
  temp2 <- read_csv(str_c(race_dir, 'ACSDP5Y', i, '.csv')) %>%
    slice(62) %>%
    select(`Label (Grouping)`, contains('Percent')) %>%
    select(-contains('Margin of Error')) %>%
    pivot_longer(contains('Percent'), names_to = 'county', values_to = 'race_percent') %>%
    filter(!is.na(race_percent)) %>%
    pivot_wider(names_from = `Label (Grouping)`, values_from = race_percent) %>%
    mutate(year = i) %>%
    clean_names()
  
  temp2$county <- str_remove(temp2$county, ' County, California!!Percent')
  
  race_percentages <- race_percentages %>%
    rbind(temp2)
}

for (i in 2017:2020) {
  temp3 <- read_csv(str_c(race_dir, 'ACSDP5Y', i, '.csv')) %>%
    slice(67) %>%
    select(`Label (Grouping)`, contains('Percent')) %>%
    select(-contains('Margin of Error')) %>%
    pivot_longer(contains('Percent'), names_to = 'county', values_to = 'race_percent') %>%
    filter(!is.na(race_percent)) %>%
    pivot_wider(names_from = `Label (Grouping)`, values_from = race_percent) %>%
    mutate(year = i) %>%
    clean_names()
  
  temp3$county <- str_remove(temp3$county, ' County, California!!Percent')
  
  race_percentages <- race_percentages %>%
    rbind(temp3)
}

race_percentages$white <- str_remove(race_percentages$white, '%')

race_percentages <- race_percentages %>%
  mutate(white = as.numeric(white))

# Political Geography Data. ####


pols_dir <- str_c(raw_data_dir, 'Pols/')

pols <- tibble()

for (i in c(2010:2016, 2018)) {
  temp4 <- read_xls(str_c(pols_dir, 'pols ', i, '.xls')) %>%
    clean_names() %>%
    select(county, democratic, registered) %>%
    remove_empty('rows') %>%
    filter(county != 'Percent') %>%
    mutate(percent_dem = democratic / registered * 100) %>%
    select(county, percent_dem) %>%
    mutate(year = i)
  
  pols <- pols %>%
    rbind(temp4)
}

for (i in c(2017, 2019, 2020)) {
  temp5 <- read_xlsx(str_c(pols_dir, 'pols ', i, '.xlsx')) %>%
    clean_names() %>%
    select(county, democratic, total_registered) %>%
    remove_empty('rows') %>%
    filter(county != 'Percent') %>%
    mutate(percent_dem = democratic / total_registered * 100) %>%
    select(county, percent_dem) %>%
    mutate(year = i)
  
  pols <- pols %>%
    rbind(temp5)
}

pols <- pols %>%
  filter(county != 'State Total')

# Climate Data. ####

fips <- read_csv('https://www2.census.gov/geo/docs/reference/codes/files/st06_ca_cou.txt',
                 col_names = F) %>%
  rename('county' = X4,
         'division_number' = X3) %>%
  select(county, division_number)

fips$county <- str_remove(fips$county, ' County')

hdd <- read_delim(str_c(raw_data_dir, 'climdiv-hddccy-v1.0.0-20220304.txt'),
                  delim = '.', col_names = F) %>%
  separate(X1, into = c('state_code', 'temp'), sep = 2) %>%
  filter(state_code == '04') %>%
  separate(temp, into = c('division_number', 'temp'), sep = 3) %>%
  separate(temp, into = c('element_code', 'temp'), sep = 2) %>%
  separate(temp, into = c('year', 'January')) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% 2010:2020) %>%
  rename('February' = X2,
         'March' = X3,
         'April' = X4,
         'May' = X5,
         'June' = X6,
         'July' = X7,
         'August' = X8,
         'September' = X9,
         'October' = X10,
         'November' = X11,
         'December' = X12) %>%
  mutate(January = as.numeric(January),
         February = as.numeric(February),
         March = as.numeric(March),
         April = as.numeric(April),
         May = as.numeric(May),
         June = as.numeric(June),
         July = as.numeric(July),
         August = as.numeric(August),
         September = as.numeric(September),
         October = as.numeric(October),
         November = as.numeric(November),
         December = as.numeric(December)) %>%
  select(-X13) %>%
  pivot_longer(cols = `January`:`December`, names_to = 'month', values_to = 'hdd') %>%
  group_by(division_number, year) %>%
  summarize(hdd = mean(hdd)) %>%
  ungroup() %>%
  left_join(fips) %>%
  select(county, year, hdd)

cdd <- read_delim(str_c(raw_data_dir, 'climdiv-cddccy-v1.0.0-20220304.txt'),
                  delim = '.', col_names = F) %>%
  separate(X1, into = c('state_code', 'temp'), sep = 2) %>%
  filter(state_code == '04') %>%
  separate(temp, into = c('division_number', 'temp'), sep = 3) %>%
  separate(temp, into = c('element_code', 'temp'), sep = 2) %>%
  separate(temp, into = c('year', 'January')) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% 2010:2020) %>%
  rename('February' = X2,
         'March' = X3,
         'April' = X4,
         'May' = X5,
         'June' = X6,
         'July' = X7,
         'August' = X8,
         'September' = X9,
         'October' = X10,
         'November' = X11,
         'December' = X12) %>%
  mutate(January = as.numeric(January),
         February = as.numeric(February),
         March = as.numeric(March),
         April = as.numeric(April),
         May = as.numeric(May),
         June = as.numeric(June),
         July = as.numeric(July),
         August = as.numeric(August),
         September = as.numeric(September),
         October = as.numeric(October),
         November = as.numeric(November),
         December = as.numeric(December)) %>%
  select(-X13) %>%
  pivot_longer(cols = `January`:`December`, names_to = 'month', values_to = 'cdd') %>%
  group_by(division_number, year) %>%
  summarize(cdd = mean(cdd)) %>%
  ungroup() %>%
  left_join(fips) %>%
  select(county, year, cdd)

# AHHHHHH
max_tm <- read_csv(str_c(raw_data_dir, 'climdiv-tmaxcy-v1.0.0-20220304.txt'), 
                     col_names = F) %>%
  separate(X1, into = c('state_code', 'temp'), sep = 2) %>%
  filter(state_code == '04') %>%
  separate(temp, into = c('division_number', 'temp'), sep = 3) %>%
  separate(temp, into = c('element_code', 'temp'), sep = 2) %>%
  separate(temp, into = c('year', 'temp'), sep = 4) %>%
  separate(temp, into = c('January', 'temp'), sep = 7) %>%
  separate(temp, into = c('February', 'temp'), sep = 7) %>%
  separate(temp, into = c('March', 'temp'), sep = 7) %>%
  separate(temp, into = c('April', 'temp'), sep = 7) %>%
  separate(temp, into = c('May', 'temp'), sep = 7) %>%
  separate(temp, into = c('June', 'temp'), sep = 7) %>%
  separate(temp, into = c('July', 'temp'), sep = 7) %>%
  separate(temp, into = c('August', 'temp'), sep = 7) %>%
  separate(temp, into = c('September', 'temp'), sep = 7) %>%
  separate(temp, into = c('October', 'temp'), sep = 7) %>%
  separate(temp, into = c('November', 'December'), sep = 7) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% 2010:2020) %>%
  mutate(January = as.numeric(January),
         February = as.numeric(February),
         March = as.numeric(March),
         April = as.numeric(April),
         May = as.numeric(May),
         June = as.numeric(June),
         July = as.numeric(July),
         August = as.numeric(August),
         September = as.numeric(September),
         October = as.numeric(October),
         November = as.numeric(November),
         December = as.numeric(December)) %>%
  pivot_longer(cols = `January`:`December`, names_to = 'month', values_to = 'max_tm') %>%
  group_by(division_number, year) %>%
  summarize(max_tm = mean(max_tm)) %>%
  ungroup() %>%
  left_join(fips) %>%
  select(county, year, max_tm)

min_tm <- read_csv(str_c(raw_data_dir, 'climdiv-tmincy-v1.0.0-20220304.txt'), 
                   col_names = F) %>%
  separate(X1, into = c('state_code', 'temp'), sep = 2) %>%
  filter(state_code == '04') %>%
  separate(temp, into = c('division_number', 'temp'), sep = 3) %>%
  separate(temp, into = c('element_code', 'temp'), sep = 2) %>%
  separate(temp, into = c('year', 'temp'), sep = 4) %>%
  separate(temp, into = c('January', 'temp'), sep = 7) %>%
  separate(temp, into = c('February', 'temp'), sep = 7) %>%
  separate(temp, into = c('March', 'temp'), sep = 7) %>%
  separate(temp, into = c('April', 'temp'), sep = 7) %>%
  separate(temp, into = c('May', 'temp'), sep = 7) %>%
  separate(temp, into = c('June', 'temp'), sep = 7) %>%
  separate(temp, into = c('July', 'temp'), sep = 7) %>%
  separate(temp, into = c('August', 'temp'), sep = 7) %>%
  separate(temp, into = c('September', 'temp'), sep = 7) %>%
  separate(temp, into = c('October', 'temp'), sep = 7) %>%
  separate(temp, into = c('November', 'December'), sep = 7) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% 2010:2020) %>%
  mutate(January = as.numeric(January),
         February = as.numeric(February),
         March = as.numeric(March),
         April = as.numeric(April),
         May = as.numeric(May),
         June = as.numeric(June),
         July = as.numeric(July),
         August = as.numeric(August),
         September = as.numeric(September),
         October = as.numeric(October),
         November = as.numeric(November),
         December = as.numeric(December)) %>%
  pivot_longer(cols = `January`:`December`, names_to = 'month', values_to = 'min_tm') %>%
  group_by(division_number, year) %>%
  summarize(min_tm = mean(min_tm)) %>%
  ungroup() %>%
  left_join(fips) %>%
  select(county, year, min_tm)


# Putting It Together. ####

data <- data %>%
  left_join(fit) %>%
  left_join(population) %>%
  left_join(gdp) %>%
  left_join(income) %>%
  left_join(capacity) %>%
  left_join(consumption) %>%
  left_join(race_percentages) %>%
  left_join(pols) %>%
  left_join(hdd) %>%
  left_join(cdd) %>%
  left_join(min_tm) %>%
  left_join(max_tm)

# Columns with missing values: emissions, capacity_mw, and net_m_wh.

data[is.na(data)] = 0

data <- data %>%
  mutate(gdp = real_gdp / pop) %>%
  select(county, year, emissions, net_m_wh, tariff, gdp, total_consumption, 
         white, percent_dem, hdd, cdd, min_tm, max_tm) %>%
  rename('consumption' = total_consumption,
         'dem' = percent_dem,
         'capacity' = net_m_wh)

data <- data[order(data$county, data$year),]


# Export dataset. ####

writexl::write_xlsx(data, str_c(wd, '/Dataset.xlsx'))
