library(tidyverse)
library(ggplot2)
library(ppsr)
library(janitor)
library(comtradr)

######################## reading in data ###############################
# contributions <- read_csv("Data/us_contributions_dataset.csv")
# contributions <- contributions[,-(9:ncol(contributions))]
# contributions$mismanaged_waste <- as.numeric(contributions$mismanaged_waste) / 100
codes <- read_csv("Data/ISO_codes_countries.csv") %>% clean_names() %>% 
  select(-country_comments, - iso2_digit_alpha) %>%
  filter(end_valid_year == "Now")
codes[codes$iso3_digit_alpha == "CIV", "country_name_full"] = "Côte d'Ivoire"
codes[codes$iso3_digit_alpha == "CIV", "country_name_abbreviation"] = "Côte d'Ivoire"
codes[codes$iso3_digit_alpha == "CUW", "country_name_full"] = "Curaçao"
codes[codes$iso3_digit_alpha == "CUW", "country_name_abbreviation"] = "Curaçao"
codes[codes$iso3_digit_alpha == "BLM", "country_name_full"] = "Saint Barthélemy"
codes[codes$iso3_digit_alpha == "BLM", "country_name_abbreviation"] = "Saint Barthélemy"


# contributions <- left_join(contributions, codes, by = c("country" = "country_name_full"))

contributions <- read_csv("Data/us_contributions_cleaned.csv")
eu_members <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", 
                "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", 
                "ROU", "SVK", "SVN", "ESP", "SWE")



# mismanaged <- read_csv("Data/mismanagedPlasticWaste.csv") %>% select(Code, mismanaged) %>% rename(iso3c = Code)
# mismanaged$mismanaged <- mismanaged$mismanaged / 100
plastic_recycling <- read_csv("Data/plastic_recycling.csv", col_names = T)
plastic_recycling$plastic_recycling_data <- plastic_recycling$plastic_recycling_data/100
regions <- read_csv("Data/regions.csv")

wbData_full <- read_csv("Data/country_level_data_0.csv") %>%
  mutate(plastic_waste_wb = total_msw_total_msw_generated_tons_year * composition_plastic_percent, 
         recycled = waste_treatment_recycling_percent/100) %>%
  rename(population = population_population_number_of_people, 
         total_msw = total_msw_total_msw_generated_tons_year, 
         waste_collection = waste_collection_coverage_total_percent_of_population) %>%
  left_join(select(contributions, iso3_digit_alpha, mismanaged_waste, plastic_waste), by = c("iso3c" = "iso3_digit_alpha")) %>% 
  filter(!is.na(mismanaged_waste))



wbData <- select(wbData_full, names(wbData_full)[1:5],
                 composition_plastic_percent,
                 gdp,
                 population, 
                 total_msw,
                 waste_collection, 
                 recycled, 
                 plastic_waste, 
                 region_id, 
                 mismanaged_waste) %>%
  left_join(regions) %>%
  filter(!is.na(plastic_waste)) %>%
  left_join(select(plastic_recycling, iso3c, plastic_recycling_data)) %>%
  mutate(gdp_per_capita = gdp/population)
wbData <- wbData %>% filter(plastic_waste != "NR") 
wbData$plastic_waste <- as.numeric(wbData$plastic_waste)
countries <- wbData$iso3c

# comtrade_re_imp <- data.frame(exporter = character(), importer = character(), re_imp_q = numeric(), commod_code = character())
# comtrade_re_exp <- data.frame(exporter = character(), importer = character(), re_exp_q = numeric(), commod_code = character())
# for(i in seq(from = 1, to = length(countries), by = 5)){
#   print(i)
#   partner_set = codes[codes$iso3_digit_alpha %in% (countries[i:(i + 4)][!is.na(countries[i:(i + 4)])]),]$country_name_abbreviation
# 
# 
#   comtrade_re_imp_i <- ct_search(reporters = "all", partners = partner_set, trade_direction = "re_imports", freq = "annual", start_date = "2017", end_date = "2017", commod_codes = c("391510", "391520", "391530", "391590")) %>%
#     rename(exporter = partner_iso, importer = reporter_iso, re_imp_q = qty) %>%
#     select(importer, exporter, re_imp_q, commodity_code)
#   comtrade_re_imp <- rbind(comtrade_re_imp, comtrade_re_imp_i)
#   Sys.sleep(1)
#   comtrade_re_exp_i <- ct_search(reporters = "all", partners = partner_set, trade_direction = "re_exports", freq = "annual", start_date = "2017", end_date = "2017", commod_codes = c("391510", "391520", "391530", "391590")) %>%
#     rename(importer = partner_iso, exporter = reporter_iso, re_exp_q = qty) %>%
#     select(importer, exporter, re_exp_q, commodity_code)
#   comtrade_re_exp <- rbind(comtrade_re_exp, comtrade_re_exp_i)
#   Sys.sleep(1)
# }
# #imp_swap <- comtrade_re_imp %>% rename(exporter = importer, importer = exporter)
# re_exp_data <- full_join(unique(comtrade_re_imp), unique(comtrade_re_exp)) %>%
#   mutate(quantity = ifelse(is.na(re_imp_q), re_exp_q, ifelse(is.na(re_exp_q), re_imp_q, max(re_exp_q, re_imp_q))))
# re_exp_data[is.na(re_exp_data$quantity), "quantity"] <- max(re_exp_data[is.na(re_exp_data$quantity), "re_exp_q"], re_exp_data[is.na(re_exp_data$quantity), "re_imp_q"])
# write_csv(re_exp_data, "Data/re_traded_reconciled2017.csv")
# write_csv(comtrade_re_exp, "Data/re_exp_reconciled2017.csv")
# write_csv(comtrade_re_imp, "Data/re_imp_reconciled2017.csv")

################################ cleaning trade data ######################################


countries_trade <- read_csv("Data/ISO_codes_countries.csv") %>% clean_names() %>% select(country_code, iso3_digit_alpha)
eu_mismanagement <- data.frame(year = 1995:2019, r = numeric(25))

for(y in 1995:2019){

  trade_data <- clean_baci(year = y)
  trade_total_EU_agg <- trade_eu_agg(trade = trade_data)
  trade_total_EU_agg[is.na(trade_total_EU_agg$quantity), 3] <- 0
  export_totals <- trade_total_EU_agg %>%
    group_by(exporter) %>%
    summarize(exports = sum(quantity))
  import_totals <- trade_total_EU_agg %>%
    group_by(importer) %>%
    summarize(imports = sum(quantity))
  trade_probs <- left_join(trade_total_EU_agg, export_totals) %>%
    mutate(probability = quantity / exports)
  trade_probs <- rename(trade_probs, from = exporter, to = importer)
  # assign(paste("trade_probs", y, sep = ""), trade_probs)
  # trade_probs_yearly <- get(paste("trade_probs", y, sep = ""))
  
  write_csv(trade_probs, paste("Data/trade_clean/trade_probs", y, ".csv", sep = ""))
  
  eu_mis <- compute_EU_mismanagement(year = y, trade = trade_data)
  eu_mismanagement[eu_mismanagement$year == y, 2] = eu_mis
  
}
write_csv(eu_mismanagement, "Data/eu_mismanagement_rates.csv")

# trade2018_total <- trade2018 %>%
#   group_by(exporter, importer) %>%
#   summarize(quantity = sum(q) * 1.10231) %>%
#   filter(exporter %in% countries, importer %in% countries)
# 
# exports_to_EU <- trade2018_total %>%
#   filter(importer %in% eu_members) %>%
#   group_by(exporter) %>%
#   summarize(quantity = sum(quantity)) %>%
#   mutate(importer = "EU") %>%
#   filter(!(exporter %in% eu_members)) %>%
#   select(exporter, importer, quantity)
# 
# imports_from_EU <- trade2018_total %>%
#   filter(exporter %in% eu_members) %>%
#   group_by(importer) %>%
#   summarize(quantity = sum(quantity)) %>%
#   mutate(exporter = "EU") %>%
#   filter(!(importer %in% eu_members)) %>%
#   select(exporter, importer, quantity)
# 
# trade2018_total_EU_agg <- trade2018_total %>%
#   filter(!((importer %in% eu_members) | (exporter %in% eu_members))) %>%
#   ungroup() %>%
#   rbind(exports_to_EU, imports_from_EU)

# convert quantities to probabilities
# export_totals <- trade2018_total_EU_agg %>%
#   group_by(exporter) %>%
#   summarize(exports = sum(quantity)) 
# import_totals <- trade2018_total_EU_agg %>%
#   group_by(importer) %>%
#   summarize(imports = sum(quantity)) 








############################# find weighted average of EU mismanagement rate with total waste management as the weight variable ###################

# trade_regional <- trade2018 %>% 
#   group_by(importer, exporter) %>%
#   summarize(quantity = sum(q) * 1.10231) %>%
#   left_join(select(wbData, iso3c, region_id), 
#             by = c("exporter" = "iso3c")) %>%
#   rename(exporter_region = region_id)%>% 
#   left_join(select(wbData, iso3c, region_id), 
#             by = c("importer" = "iso3c")) %>%
#   rename(importer_region = region_id) 
# EU_imports <- trade_regional %>%
#   filter((importer %in% eu_members)) %>%
#   group_by(importer) %>%
#   summarize(quantity_imp = sum(quantity))
# EU_exports <- trade_regional %>%
#   filter((exporter %in% eu_members)) %>%
#   group_by(exporter) %>%
#   summarize(quantity_exp = sum(quantity))
# EU_trade <- left_join(EU_imports, EU_exports, by = c("importer" = "exporter")) %>%
#   rename(country = importer) %>%
#   mutate(net = quantity_imp - quantity_exp)
# EU_waste <- EU_trade %>% left_join(select(wbData, iso3c, plastic_waste, mismanaged_waste), by = c("country" = "iso3c")) %>%
#   mutate(total_managed_waste = net + plastic_waste)
# 
# EU_mismanagement_rate <- weighted.mean(EU_waste$mismanaged_waste, EU_waste$total_managed_waste)


############################# aggregate EU values #############################

wb_EU <- filter(wbData, iso3c %in% eu_members)
EU_data <- data.frame(iso3c = "EU", 
                      region_id = "ECS", 
                      country_name = "European Union",
                      income_id = "HIC", 
                      gdp = sum(wb_EU$gdp), 
                      total_msw = sum(wb_EU$total_msw), 
                      recycled = 0.30, 
                      plastic_waste = sum(wb_EU$plastic_waste), 
                      mismanaged_waste = NA, 
                      region = "Europe", 
                      sub_region = "Western Europe", 
                      plastic_recycling_data = 0.30, 
                      population = sum(wb_EU$population),
                      gdp_per_capita = sum(wb_EU$gdp) /  sum(wb_EU$population))


wbData_EU_agg <- wbData %>%
  filter(!(iso3c %in% eu_members)) %>%
  select(names(EU_data)) %>%
  rbind(EU_data) 

countries_EU_agg <- wbData_EU_agg$iso3c


############################# domestic trade fraction and re-export fraction #############################


# trade_fraction <- left_join(select(wbData_EU_agg, iso3c, plastic_waste), 
#                             trade_dom_re, 
#                             by = c("iso3c" = "exporter")) %>%
#   left_join(import_totals, by = c("iso3c" = "importer"))
# trade_fraction[is.na(trade_fraction)] <- 0
# trade_fraction$re_export_total[trade_fraction$re_export_total > trade_fraction$imports]<- trade_fraction$imports
# trade_fraction <- trade_fraction %>%
#   mutate(trade_frac = dom_export_total / as.numeric(plastic_waste), 
#          re_exp_frac = re_export_total / imports)  #change decimal to percent, convert exports from tons to kg
# 
# trade_fraction[is.nan(trade_fraction$re_exp_frac), "re_exp_frac"] <- 0



#trade2018_probs <- left_join(trade2018_probs, select(trade_fraction, iso3c, trade_frac), by = c("exporter" = "iso3c"))
#trade2018_probs$probability <- trade2018_probs$probability * trade2018_probs$trade_frac
#trade2018_probs <- select(trade2018_probs, -trade_frac)
# trade2018_probs <- rename(trade2018_probs, from = exporter, to = importer)


# write_csv(trade2018_total_EU_agg, "Data/trade2018.csv")
# write_csv(trade2018_probs, "Data/trade2018_probs.csv")


############################## exploratory analysis ############################################
# ggplot(wbData_EU_agg) + geom_boxplot(aes(x = income_id, y = plastic_recycling_data))
# ggplot(filter(wbData_EU_agg, gdp_per_capita < 30000)) + geom_point(aes(x = gdp_per_capita, y = mismanaged_waste)) + geom_smooth(aes(x = gdp_per_capita, y = mismanaged_waste))
# ggplot(filter(wbData_EU_agg, gdp_per_capita > 30000)) + geom_point(aes(x = gdp_per_capita, y = mismanaged_waste))
# ggplot(filter(wbData_EU_agg, gdp_per_capita < 30000)) + geom_point(aes(x = waste_collection, y = mismanaged_waste)) + geom_smooth(aes(x = waste_collection, y = mismanaged_waste))
# ggplot(filter(wbData_EU_agg, gdp_per_capita < 30000)) + geom_point(aes(x = gdp_per_capita, y = waste_collection, color = mismanaged_waste))
# 
# ############################## mismanagement regression model ########################################
# mismanagement_model <- lm(data = wbData_EU_agg, formula = mismanaged_waste ~ gdp_per_capita  + waste_collection)
# summary(mismanagement_model)
# 
# ############################## recycling rate regression modeling ####################################
# wbData_EU_agg_no_india <- filter(wbData_EU_agg, iso3c != "IND")
# ggplot(wbData_EU_agg_no_india) + geom_jitter(aes(x = region, y = recycled)) + geom_smooth(aes(x = region, y = recycled), method = "lm") + labs(title = "Plastic recycling vs MSW recycling rates") + theme(text = element_text(size = 18))
# 
# recycling_model <- lm(data = wbData_EU_agg, formula = plastic_recycling_data ~ gdp_per_capita)
# summary(recycling_model)
# 
# reg_data <- wbData_EU_agg %>% select(iso3c, country_name, plastic_recycling_data, recycled, income_id, region, sub_region) %>%
#   mutate(estimated_plastic_recycling = predict(recycling_model, newdata = wbData_EU_agg), 
#          residual = estimated_plastic_recycling - plastic_recycling_data)
# 
# reg_data_summary <- reg_data %>%
#   group_by(sub_region) %>%
#   summarize(n = n())
# 
# ggplot(reg_data) + geom_density(aes(x = residual), size = 1.5) + labs(title = "Density of model residuals") + theme(text = element_text(size = 18))
# sd(reg_data$residual)
# mean(reg_data$residual)
# 
# 
# ######## waste generation and recycling 
# 
# plasticWasteGeneration <- wbData_EU_agg %>%
#   select(iso3c, plastic_waste)
# 




############################# create rate table, real data ####################################

#wbData_EU_agg$recycled <- ifelse(is.na(wbData_EU_agg$plastic_recycling_data), wbData_EU_agg$recycled, wbData_EU_agg$plastic_recycling_data)


#real data
rates <- wbData_EU_agg %>%
  rename(C1_rate = plastic_recycling_data, 
         mismanagement_rate = mismanaged_waste, 
         waste_generation = plastic_waste,
         income_level = income_id) #%>%
#   left_join(export_totals, by = c("iso3c" = "exporter")) %>%
#   left_join(import_totals, by = c("iso3c" = "importer")) 
# rates$imports[is.na(rates$imports)] <- 0
# rates$exports[is.na(rates$exports)] <- 0
  
rates <- rates %>% 
  mutate(fraction_recycling_chemical = rep(0, times = length(countries_EU_agg)),
         C2_rate = C1_rate, 
         I_mismanagement_min = ifelse(wbData_EU_agg$mismanaged_waste >= 0.2, .25, 0),
         I_mismanagement_max = ifelse(wbData_EU_agg$mismanaged_waste >= 0.2, .75, 0))
rates[rates$iso3c == "EU", "I_mismanagement_min"] = 0
rates[rates$iso3c == "EU", "I_mismanagement_max"] = 0
# rates <- data.frame(country = countries_EU_agg,
#                     C1_rate = wbData_EU_agg$plastic_recycling_data,
#                     C2_rate = wbData_EU_agg$plastic_recycling_data,
#                     mismanagement_rate = wbData_EU_agg$mismanaged_waste, 
#                     I_mismanagement_min = ifelse(wbData_EU_agg$mismanaged_waste >= 0.2, .25, 0),
#                     I_mismanagement_max = ifelse(wbData_EU_agg$mismanaged_waste >= 0.2, .75, 0),
#                     re_export_fraction = trade_fraction$re_exp_frac,
#                     fraction_recycling_chemical = rep(0, times = length(countries_EU_agg)),
#                     export_total = trade_fraction$trade_frac, 
#                     waste_generation = wbData_EU_agg$plastic_waste, 
#                     income_level = wbData_EU_agg$income_id, 
#                     region = wbData_EU_agg$region)


write_csv(rates, "Data/rates_real.csv")





############################### functions ###################################

clean_baci <- function(year){
  
  fname = paste("Data/BACI_raw/BACI_HS92_Y", year, "_V202102.csv", sep = "")
  trade <- read_csv(fname)
  trade$k <- as.numeric(trade$k)
  trade <- trade %>%
    filter(substr(k, 1, 4) == 3915)  %>% rename(exporter = i, importer = j)
  trade <- trade %>%  left_join(countries_trade, by = c("exporter" = "country_code")) %>%
    select(-exporter, t, v) %>%
    rename(exporter = iso3_digit_alpha) %>%
    left_join(countries_trade, by = c("importer" = "country_code")) %>%
    select(-importer) %>%
    rename(importer = iso3_digit_alpha)
  

  return(trade)
}


trade_eu_agg <- function(trade){
  
  trade_total <- trade %>%
    group_by(exporter, importer) %>%
    summarize(quantity = sum(q) * 1.10231) %>%
    filter(exporter %in% countries, importer %in% countries)
  
  exports_to_EU <- trade_total %>%
    filter(importer %in% eu_members) %>%
    group_by(exporter) %>%
    summarize(quantity = sum(quantity)) %>%
    mutate(importer = "EU") %>%
    filter(!(exporter %in% eu_members)) %>%
    select(exporter, importer, quantity)
  
  imports_from_EU <- trade_total %>%
    filter(exporter %in% eu_members) %>%
    group_by(importer) %>%
    summarize(quantity = sum(quantity)) %>%
    mutate(exporter = "EU") %>%
    filter(!(importer %in% eu_members)) %>%
    select(exporter, importer, quantity)
  
  trade_total_EU_agg <- trade_total %>%
    filter(!((importer %in% eu_members) | (exporter %in% eu_members))) %>%
    ungroup() %>%
    rbind(exports_to_EU, imports_from_EU)
  
  return(trade_total_EU_agg)
  
}

compute_EU_mismanagement <- function(year, trade){
  
  trade_regional <- trade %>% 
    group_by(importer, exporter) %>%
    summarize(quantity = sum(q) * 1.10231) %>%
    left_join(select(wbData, iso3c, region_id), 
              by = c("exporter" = "iso3c")) %>%
    rename(exporter_region = region_id)%>% 
    left_join(select(wbData, iso3c, region_id), 
              by = c("importer" = "iso3c")) %>%
    rename(importer_region = region_id) 
  EU_imports <- trade_regional %>%
    filter((importer %in% eu_members)) %>%
    group_by(importer) %>%
    summarize(quantity_imp = sum(quantity))
  EU_exports <- trade_regional %>%
    filter((exporter %in% eu_members)) %>%
    group_by(exporter) %>%
    summarize(quantity_exp = sum(quantity))
  # EU_trade <- left_join(EU_imports, EU_exports, by = c("importer" = "exporter")) %>%
  #   rename(country = importer) %>%
  #   mutate(net = quantity_imp - quantity_exp)
  EU_waste <- left_join(select(wbData, iso3c, plastic_waste, mismanaged_waste), EU_imports, by = c("iso3c" = "importer")) %>%
    left_join(EU_exports, by = c("iso3c" = "exporter")) %>%
    filter(iso3c %in% eu_members) 
  EU_waste[is.na(EU_waste)] <- 0
  EU_waste <- EU_waste %>% 
    mutate(net = quantity_imp - quantity_exp) %>%
    mutate(total_managed_waste = net + plastic_waste)
  
  EU_mismanagement_rate <- weighted.mean(EU_waste$mismanaged_waste, EU_waste$total_managed_waste)
  
  return(EU_mismanagement_rate)
  
}


a = read_csv("Data/Baci_raw/BACI_HS92_Y2019_V202102.csv")
a2 = left_join(a, codes[, 1:2], by = c("i"="country_code"))
a2 = rename(a2, exporter = country_name_full)
a2 = left_join(a2, codes[, 1:2], by = c("j"="country_code"))
a2 = rename(a2, importer = country_name_full)
