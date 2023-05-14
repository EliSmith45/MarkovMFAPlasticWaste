library(tidyverse)
library(janitor)

countries <- read_csv("Data/ISO_codes_countries.csv") %>% clean_names() %>% select(country_code, iso3_digit_alpha)
trade2018 <- read_csv("Data/BACI_HS92_Y2018_V202001.csv")
trade2018$k <- as.numeric(trade2018$k)
trade2018 <- trade2018 %>% 
  filter(substr(k, 1, 4) == 3915)  %>% rename(exporter = i, importer = j) 
trade2018 <- trade2018 %>%  left_join(countries, by = c("exporter" = "country_code")) %>%
  select(-exporter, t, v) %>%
  rename(exporter = iso3_digit_alpha) %>%
  left_join(countries, by = c("importer" = "country_code")) %>%
  select(-importer) %>%
  rename(importer = iso3_digit_alpha)

trade2018_total <- trade2018 %>%
  group_by(exporter, importer) %>%
  summarize(quantity = sum(q)) 


write_csv(trade2018_total, "Data/trade2018.csv")

