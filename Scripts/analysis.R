library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(weights)
library(data.table)
library(Cairo)
options(dplyr.summarise.inform = FALSE)
pal1 <- brewer.pal(n = 9, name = "YlGnBu")[2:7]

scenario_codes <- data.frame(scenario = c("baseline",
                                          "20% import recycling max",
                                          "no trade, landfill",
                                          "no trade, recycle",
                                          "no trade, landfill, use capacity",
                                          "no trade, recycle, use capacity",
                                          "increase sorting",
                                          "increase sorting & yield",
                                          "50%, mechanical",
                                          "50%, chemical",
                                          "increase mech, chem, yield"),
                             scenario_code = c("B.1",
                                               "B.2",
                                               paste0("T.", as.character(1:4)),
                                               paste0("R.", as.character(1:5))))

rate_data <- read_csv("Data/rates_real.csv") %>% as.data.frame() %>%
  rename(country = iso3c) %>%
  arrange(country) %>%
  select(country, 
         waste_generation,
         recycled, 
         mismanagement_rate,
         C1_rate,
         fraction_recycling_chemical,
         I_mismanagement_min,
         I_mismanagement_max)



rate_data$C1_rate[rate_data$country == "CHN"] = 0.3 # https://www.reuters.com/article/uk-china-environment-plastics-idUKKBN28A13X
rate_data[rate_data$country == "IND", "C1_rate"] = 0.1
eu_mismanagement <- read_csv("Data/eu_mismanagement_rates.csv")
countries <- rate_data$country


twg <- as.matrix(rate_data[order(rate_data$country),"waste_generation"])
rownames(twg) <- rate_data[,"country"]
twg

trade_flow_data <- read_csv(paste("Data/trade_clean/trade_probs", 2019, ".csv", sep = "")) %>%
  filter(from %in% countries, to %in% countries) %>%
  left_join(select(rate_data, country, mismanagement_rate), by = c("from" = "country")) %>%
  rename(exporter_mismanagement = mismanagement_rate) %>%
  left_join(select(rate_data, country, mismanagement_rate), by = c("to" = "country")) %>%
  rename(importer_mismanagement = mismanagement_rate) %>%
  filter(from != "NAM") %>% #data contains zero flows for NAM
  as.data.frame()

trade_flow_data[trade_flow_data$from == "EU", "exporter_mismanagement"] = eu_mismanagement[eu_mismanagement$year == 2019, "r"]
trade_flow_data[trade_flow_data$to == "EU", "importer_mismanagement"] = .05741

filled <- baseline_fill_mean(2019, trade_flow_data, rate_data)
################# no trade scenarios ###########################                                               
results <- read_csv("Result_data/all_scenarios_corrected_trade.csv")  %>% filter(iter != 10000)
trade_results <- results %>% filter(scenario %in% c("no trade, landfill, use capacity",
                         "no trade, recycle, use capacity",
                         "no trade, landfill",
                         "no trade, recycle",
                         "baseline", 
                         "20% import recycling max"))
trade_results$scenario <- factor(trade_results$scenario,
                                 levels = c("baseline",
                                            "20% import recycling max",
                                            "no trade, landfill",
                                            "no trade, recycle",
                                            "no trade, landfill, use capacity",
                                            "no trade, recycle, use capacity"))



trade_long <- trade_results %>%
  select(-total_waste) %>%
  pivot_longer(names_to = "Type", values_to = "Quantity", cols = 2:3) %>%
  mutate(Quantity = Quantity / 1000000) #units in Mt

  
  
trade_diffs <- rbind(mutate(trade_long, baseline_scenario = "B.1"), mutate(trade_long, baseline_scenario = "B.2"))  %>% 
  arrange(-as.numeric(scenario)) %>%
  pivot_wider(id_cols = c(1, 8, 10), names_from = "scenario", values_from = "Quantity") 

trade_diffs <- trade_diffs %>%
  mutate(baseline = ifelse(baseline_scenario == "B.1", baseline, `20% import recycling max`),
         across(.cols = 4:7, ~ baseline - .x )) %>%
  select(-c("baseline", "20% import recycling max")) %>%
  pivot_longer(cols = 4:7, names_to = "scenario", values_to = "Quantity") %>%
  left_join(scenario_codes)
trade_diffs[trade_diffs$Type == "total_mismanaged", "Type"] <- "Mismanaged"
trade_diffs[trade_diffs$Type == "virgin_waste", "Type"] <- "Virgin"

trade_percent_change <- rbind(mutate(trade_long, baseline_scenario = "B.1"), mutate(trade_long, baseline_scenario = "B.2"))  %>% 
  arrange(-as.numeric(scenario)) %>%
  pivot_wider(id_cols = c(1, 8, 10), names_from = "scenario", values_from = "Quantity") 
trade_percent_change <- trade_percent_change %>%
  mutate(baseline = ifelse(baseline_scenario == "B.1", baseline, trade_percent_change$"20% import recycling max"),
    across(.cols = 4:7, ~ ((baseline - .x) / baseline) * 100)) %>%
  select(-c("baseline", "20% import recycling max")) %>%
  pivot_longer(cols = 4:7, names_to = "scenario", values_to = "Quantity") %>%
  left_join(scenario_codes)
trade_percent_change[trade_percent_change$Type == "total_mismanaged", "Type"] <- "Mismanaged"
trade_percent_change[trade_percent_change$Type == "virgin_waste", "Type"] <- "Virgin"


# summaries with intervals
trade_sum <- trade_long %>% 
  left_join(scenario_codes) %>%
  group_by(scenario, scenario_code, Type) %>%
  summarise(mean = mean(Quantity),
            sd = sd(Quantity))
trade_sum[trade_sum$Type == "total_mismanaged", "Type"] <- "Mismanaged"
trade_sum[trade_sum$Type == "virgin_waste", "Type"] <- "Virgin"

trade_diffs_sum <- trade_diffs %>%
  group_by(baseline_scenario, Type, scenario, scenario_code) %>%
  summarise(mean = mean(Quantity), sd = sd(Quantity)) %>%
  mutate(lower = mean - 2*sd, upper = mean + 2*sd)
trade_percent_sum <- trade_percent_change %>%
  group_by(baseline_scenario, Type, scenario, scenario_code) %>%
  summarise(mean = mean(Quantity), sd = sd(Quantity)) %>%
  mutate(lower = mean - 2*sd, upper = mean + 2*sd)

trade_diffs_sum <- arrange(trade_diffs_sum, baseline_scenario, Type, scenario_code)
write_csv(trade_diffs_sum, "Result_data/trade_diffs_sum.csv")

trade_percent_sum <- arrange(trade_percent_sum, baseline_scenario, Type, scenario_code)
write_csv(trade_percent_sum, "Result_data/trade_percent_sum.csv")
library(ggpubr)

ggplot(trade_sum) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") + 
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  facet_wrap("Type", scale = "free_y") + 
  labs(title = "Waste quantities, no trade scenarios", y = "Quantity (Mt)", x = "Scenario") +
  theme_calc() +
  theme(text = element_text(size = 20), legend.position = "none") 

 ggplot(trade_diffs_sum) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") +
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  facet_grid(Type~baseline_scenario) +
  labs( y = "Reduction in mismanaged and virgin waste (Mt)", x = "Scenario") +
   theme_pubr() +
  # theme_cleveland() +
  geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  theme(text = element_text(size = 20), legend.position = "none",
        panel.grid.minor = element_line(colour="gray90", size=0.5)) 
p
ggplot(trade_percent_sum) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") + 
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - uncertainty, ymax = mean + uncertainty), width = 0.5, size = 1, color = "black") +
  #facet_grid(vars(rows = scena, cols = baseline_scenario), scale = "free_y") + 
  facet_wrap(vars(rows = Type, cols = baseline_scenario)) + 
  labs(title = "Waste reductions, no trade scenarios", y = "Percent reduction", x = "Scenario") +
  theme_tufte() +
  theme(text = element_text(size = 20), legend.position = "none") 

Cairo(width = 2800, height = 2000, file="Figures/trade_Mt_faceted.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()




####################### improved recycling scenarios ########################
recycling_results <- results %>% 
  filter(!(scenario %in% c("no trade, landfill, use capacity",
                           "no trade, recycle, use capacity",
                           "no trade, landfill",
                           "no trade, recycle",
                           "20% import recycling max")),
         sorting_yield != 0)
recycling_results$scenario <- factor(recycling_results$scenario, 
                                     levels = c("baseline", 
                                                "increase sorting",
                                                "increase sorting & yield",
                                                "50%, mechanical",
                                                "50%, chemical",
                                                "increase mech, chem, yield"))
rec_long <- recycling_results %>%
  select(-total_waste) %>%
  pivot_longer(names_to = "Type", values_to = "Quantity", cols = 2:3) %>%
  arrange(-as.numeric(scenario))

rec_diffs <- rec_long %>% 
  arrange(-as.numeric(scenario)) %>%
  pivot_wider(id_cols = c(1, 8), names_from = "scenario", values_from = "Quantity") %>%
  mutate(across(.cols = 3:8, ~ baseline - .x)) %>%
  select(-baseline) %>%
  pivot_longer(cols = 3:7, names_to = "scenario", values_to = "Quantity") %>%
  left_join(scenario_codes)
rec_diffs[rec_diffs$Type == "total_mismanaged", "Type"] <- "Mismanaged"
rec_diffs[rec_diffs$Type == "virgin_waste", "Type"] <- "Virgin"

rec_percent_change <- rec_long %>% 
  arrange(-as.numeric(scenario)) %>%
  pivot_wider(id_cols = c(1, 8), names_from = "scenario", values_from = "Quantity") %>%
  mutate(across(.cols = 3:8, ~ ((baseline - .x) / baseline) * 100)) %>%
  select(-baseline) %>%
  pivot_longer(cols = 3:7, names_to = "scenario", values_to = "Quantity") %>%
  left_join(scenario_codes)
rec_percent_change[rec_percent_change$Type == "total_mismanaged", "Type"] <- "Mismanaged"
rec_percent_change[rec_percent_change$Type == "virgin_waste", "Type"] <- "Virgin"




rec_sum <- rec_long %>%
  left_join(scenario_codes) %>% 
  group_by(scenario, scenario_code, Type) %>%
  summarise(mean = mean(Quantity),
            sd = sd(Quantity)) %>%
  mutate(uncertainty = 2*sd, 
         lower = mean - 2*sd, 
         upper = mean + 2*sd, 
         twg = sum(twg)) 
  
rec_sum[rec_sum$Type == "total_mismanaged", "Type"] <- "Mismanaged"
rec_sum[rec_sum$Type == "virgin_waste", "Type"] <- "Virgin"

rec_diffs_sum <- rec_diffs %>%
  group_by(scenario, scenario_code, Type) %>%
  summarise(mean = mean(Quantity), sd = sd(Quantity)) %>%
  mutate(lower = mean - 2*sd, upper = mean + 2*sd)

rec_percent_sum <- rec_percent_change %>%
  group_by(scenario, scenario_code, Type) %>%
  summarise(mean = mean(Quantity), sd = sd(Quantity)) %>%
  mutate(lower = mean - 2*sd, upper = mean + 2*sd)

rec_vwg_prop <- rec_sum %>%
  ungroup() %>%
  filter(Type == "Virgin") %>%
  select(scenario, scenario_code, mean, lower, upper, twg) %>%
  mutate(across(.cols = 3:5, ~ .x / twg))

rec_diffs_sum <- arrange(rec_diffs_sum, Type, scenario_code)
write_csv(rec_diffs_sum, "Result_data/rec_diffs_sum.csv")

rec_percent_sum <- arrange(rec_percent_sum, Type, scenario_code)
write_csv(rec_percent_sum, "Result_data/rec_percent_sum.csv")

rec_vwg_prop <- arrange(rec_vwg_prop, scenario_code)
write_csv(rec_vwg_prop, "Result_data/rec_vwg_prop.csv")


p <- ggplot(rec_sum %>% arrange(-as.numeric(scenario_code))) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") + 
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  facet_wrap("Type", scale = "free_y") + 
  labs(title = "Waste quantities, improved recycling scenarios", y = "Quantity (kg)", x = "Scenario") +
  theme_tufte() +
  theme(text = element_text(size = 20), legend.position = "none") 

p <- ggplot(rec_diffs_sum ) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") + 
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  facet_wrap("Type", scale = "free_y") + 
  labs(title = "Waste reductions, improved recycling scenarios", y = "Reduction in VWG (kg)", x = "Scenario") +
  theme_tufte() +
  theme(text = element_text(size = 20), legend.position = "none") 

ggplot(rec_percent_sum ) + geom_bar(aes(x = scenario_code, y = mean, fill = scenario), stat = "identity") + 
  facet_wrap("Type") +
  scale_fill_manual(values = pal1) +
  geom_errorbar(aes(x = scenario_code, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  labs( y = "Percent reduction in mismanaged and virgin waste", x = "Scenario") +
  theme_pubr() +
  theme_cleveland() +
  # geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  # theme(text = element_text(size = 20), legend.position = "none") +
  scale_y_continuous(breaks = breaks_pretty(7))+
  # theme(panel.grid.minor = element_line(colour="gray90", size=0.5))+
  theme(text = element_text(size = 20), legend.position = "none", axis.text.y = element_text(size = 18, hjust=1.1),  axis.text.x = element_text(angle = 45, hjust = 1, size = 18), axis.title.y = element_text(angle = 90)) 


p


Cairo(width = 2800, height = 2000, file="Figures/rec_percent.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()



################################## country level trade impact ################################
cty_results <- setDT(read_csv("Result_data/pairwise_corrected_trade_scenarios_cty.csv"))[scenario == "baseline", scenario := "B.1"][scenario == "20% import recycling max", scenario := "B.2"]  
cty_results$scenario <- factor(cty_results$scenario,
                               levels = c("B.1",
                                          "B.2",
                                          "no trade, landfill",
                                          "no trade, recycle",
                                          "no trade, landfill, use capacity",
                                          "no trade, recycle, use capacity"))
cty_spread <- cty_results[order(as.numeric(scenario)),] %>% 
  dcast(iter + country ~ scenario, value.var = "disposed") 


cty_B1_diffs <- copy(cty_spread)[, 5:8 := lapply(.SD, function(x) (B.1 - x) / B.1 * 100), .SDcols = 5:8][, -c(3,4)] 
cty_B1_diffs[,baseline_scenario := "B.1"]
cty_B2_diffs <- copy(cty_spread)[, 5:8 := lapply(.SD, function(x) (B.2 - x) / B.2 * 100), .SDcols = 5:8][, -c(3,4)]  
cty_B2_diffs[,baseline_scenario := "B.2"]

cty_diffs <- rbind(cty_B1_diffs, cty_B2_diffs)
cty_diffs <- cty_diffs[iter != 10000, ]
cty_summary <- melt(cty_diffs, measure.vars = 3:6, variable.name = "scenario", value.name = "percent_reduction")
cty_disagg_scenarios <- cty_summary[, .(mean = mean(percent_reduction),
                                        uncertainty = 2 * sd(percent_reduction)),
                                    by = c("country", "scenario", "baseline_scenario")]

cty_ave_scenario <- cty_summary[, .(mean = mean(percent_reduction),
                               sd = sd(percent_reduction),
                               lower = mean(percent_reduction) - 2 * sd(percent_reduction),
                               upper = mean(percent_reduction) + 2 * sd(percent_reduction)), by = c("country", "baseline_scenario")] %>%
  merge(y = rate_data %>% select(country, mismanagement_rate))



top_importers <- filled %>% 
  mutate(net_import_frac = (imports - modeled_exp)) %>%
  arrange(-net_import_frac) %>%
  filter(mismanagement_rate > .2) %>%
  select(country) %>%
  slice(1:10)
top_exporters <- filled %>% 
  mutate(net_import_frac = (imports - modeled_exp)) %>%
  arrange(net_import_frac) %>%
  select(country) %>%
  slice(1:10)

sum(filled %>% filter(country %in% top_importers$country) %>% select(imports) %>% sum()) / sum(filled$imports)
sum(filled %>% filter(country %in% top_importers$country[1:4]) %>% select(imports) %>% sum()) / sum(filled$imports)
sum(filled %>% filter(country %in% top_exporters$country) %>% select(exports) %>% sum()) / sum(filled$exports)


top_imp_sum <- cty_ave_scenario[country %in% top_importers$country]
top_exp_sum <- cty_ave_scenario[country %in% top_exporters$country]

getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))
ggplot(rbind(top_imp_sum, top_exp_sum) %>% filter(baseline_scenario == "B.1") ) + 
  geom_bar(aes(x = reorder(country, -mean), y = mean), fill = getPalette(20), stat = "identity") + 
 # scale_fill_manual(values = getPalette(20)) +
  geom_errorbar(aes(x = country, ymin = mean - 2 * sd, ymax = mean + 2 * sd), width = 0.5, size = 1, color = "black") +
  labs(y = "Percent reduction in total disposed waste", x = "Country") +
  theme_pubr() +
  # theme_cleveland() +
  # geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  theme(text = element_text(size = 20), legend.position = "none",
        panel.grid.minor = element_line(colour="gray90", size=0.5)) 
  theme(text = element_text(size = 20), legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) 

  
top_imp_sum_T1<- cty_disagg_scenarios[(country %in% top_importers$country) & scenario == "no trade, landfill"]
top_exp_sum_T1 <- cty_disagg_scenarios[(country %in% top_exporters$country) & scenario == "no trade, landfill"]
ggplot(rbind(top_imp_sum_T1, top_exp_sum_T1) %>% filter(baseline_scenario == "B.1") ) + 
  geom_bar(aes(x = reorder(country, -mean), y = mean), fill = getPalette(20), stat = "identity") + 
  # scale_fill_manual(values = getPalette(20)) +
  geom_errorbar(aes(x = country, ymin = mean - uncertainty, ymax = mean + uncertainty), width = 0.5, size = 1, color = "black") +
  labs(y = "Percent reduction in total disposed waste", x = "Country") +
  theme_pubr() +
  theme_cleveland() +
  # geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  # theme(text = element_text(size = 20), legend.position = "none") +
  scale_y_continuous(breaks = breaks_pretty(7))+
  # theme(panel.grid.minor = element_line(colour="gray90", size=0.5))+
  theme(text = element_text(size = 20), legend.position = "none", axis.text.y = element_text(size = 18, hjust=1.1),  axis.text.x = element_text(angle = 45, hjust = 1, size = 18), axis.title.y = element_text(angle = 90)) 

library(scales)
top_imp_sum_T4<- cty_disagg_scenarios[(country %in% top_importers$country) & scenario == "no trade, recycle, use capacity"]
top_exp_sum_T4 <- cty_disagg_scenarios[(country %in% top_exporters$country) & scenario == "no trade, recycle, use capacity"]
ggplot(rbind(top_imp_sum_T4, top_exp_sum_T4) %>% filter(baseline_scenario == "B.1") ) + 
  geom_bar(aes(x = reorder(country, -mean), y = mean), fill = getPalette(20), stat = "identity") + 
  # scale_fill_manual(values = getPalette(20)) +
  geom_errorbar(aes(x = country, ymin = mean - uncertainty, ymax = mean + uncertainty), width = 0.5, size = 1, color = "black") +
  labs(y = "Percent reduction in total disposed waste", x = "Country") +
  theme_pubr() +
  theme_cleveland() +
  # geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  # theme(text = element_text(size = 20), legend.position = "none") +
  scale_y_continuous(breaks = breaks_pretty(7))+
  # theme(panel.grid.minor = element_line(colour="gray90", size=0.5))+
  theme(text = element_text(size = 20), legend.position = "none", axis.text.y = element_text(size = 18, hjust=1.1),  axis.text.x = element_text(angle = 45, hjust = 1, size = 18), axis.title.y = element_text(angle = 90)) 



top_impT4 <- rbind(top_imp_sum_T4, top_exp_sum_T4) %>% filter(baseline_scenario == "B.1") %>% mutate(sc = "T4")
top_impT1 <- rbind(top_imp_sum_T1, top_exp_sum_T1) %>% filter(baseline_scenario == "B.1") %>% mutate(sc = "T1")

top_imp <- rbind(top_impT1, top_impT4)
ggplot(top_imp) + 
  geom_bar(aes(x = reorder(country, -mean), y = mean), fill = getPalette(40)[27], stat = "identity") + 
  facet_grid("sc") + 
  # scale_fill_manual(values = getPalette(20)) +
  geom_errorbar(aes(x = country, ymin = mean - uncertainty, ymax = mean + uncertainty), width = 0.5, size = 1, color = "black") +
  labs(y = "Percent reduction in total disposed waste", x = "Country") +
  theme_pubr() +
  theme_cleveland() +
  # geom_segment(aes(y = 0, yend = 0, x = .5, xend = 5), size = .5)+
  # theme(text = element_text(size = 20), legend.position = "none") +
  scale_y_continuous(breaks = breaks_pretty(7))+
  # theme(panel.grid.minor = element_line(colour="gray90", size=0.5))+
  theme(text = element_text(size = 20), legend.position = "none", axis.text.y = element_text(size = 18, hjust=1.1),  axis.text.x = element_text(angle = 45, hjust = 1, size = 18), axis.title.y = element_text(angle = 90)) 






Cairo(width = 2800, height = 2000, file="Figures/disposal_change.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()

top_imp_sum_scen <- cty_disagg_scenarios[country %in% top_importers$country
                                         ][, .(country, scenario, baseline_scenario, round(.SD, 1)), .SDcols = c("mean", "uncertainty")] %>% dcast(formula = country + baseline_scenario ~ scenario, value.var = c("mean", "uncertainty"))
top_imp_sum_scen <- top_imp_sum_scen[order(baseline_scenario, country)]

top_exp_sum_scen <- cty_disagg_scenarios[country %in% top_exporters$country
                                         ][, .(country, scenario, baseline_scenario, round(.SD, 1)), .SDcols = c("mean", "uncertainty")] %>% dcast(country + baseline_scenario ~ scenario, value.var = c("mean", "uncertainty"))
top_exp_sum_scen <- top_exp_sum_scen[order(baseline_scenario, country)]

write_csv(top_imp_sum_scen, "Result_data/top_imp_sum_scen.csv")
write_csv(top_exp_sum_scen, "Result_data/top_exp_sum_scen.csv")

top_impacts <- cty_disagg_scenarios[, .(country, scenario, baseline_scenario, round(.SD, 1)), .SDcols = c("mean", "uncertainty")] %>% dcast(country + baseline_scenario ~ scenario, value.var = c("mean", "uncertainty"))
top_impacts <- top_impacts[order(baseline_scenario, country)]
top_B1 <- top_impacts[baseline_scenario == "B.1",][order(-`mean_no trade, landfill`)]
top_B2 <- top_impacts[baseline_scenario == "B.2",][order(-`mean_no trade, landfill`)]


write_csv(top_B1, "Result_data/top_impacts_B1.csv")
write_csv(top_B2, "Result_data/top_impacts_B2.csv")
################################## trade impact over time ################################

trade_impact_over_time <- read_csv("Result_data/trade_impact.csv")
trade_impact <- trade_impact_over_time %>% as.data.frame() %>%
  group_by(year) %>%
  summarise(mismanaged = mean(total_mismanaged), sd_mismanaged = sd(total_mismanaged)) %>%
  ungroup() %>%
  mutate(lower = mismanaged - (3*sd_mismanaged), upper = mismanaged + (3*sd_mismanaged)) 

ggplot(trade_impact) + geom_line(aes(x = year, y = mismanaged/1000000), size = 1.25, color = "red") + 
  geom_line(aes(x = year, y = lower/1000000), size = .25, color = "blue") + 
  geom_line(aes(x = year, y = upper/1000000), size = .25, color = "blue") + 
  labs(title = "Impact of trade network, 1995-2019", y = "Mismanaged waste (Mt)") +  
  scale_x_continuous(breaks = c(seq(1995, 2015, 5), 2019)) +
  theme(text = element_text(size = 18))


################ benefit of chemical over mechanical recycling as a function of r ######################

rvals <- data.frame(r = rep(seq(0, .99, .01), 2),
                    recycling_type = c(rep("mechanical recycling only", 100), rep("chemical recycling only", 100))) %>%
  mutate(generation_throughput = ifelse(recycling_type == "mechanical recycling only", 1 + r + r^2, 1 / (1 - r)),
         virgin_waste = 1 / generation_throughput,
         virgin_waste_avoided = 1 - virgin_waste)


ggplot(rvals) + geom_point(aes(x = r, y = virgin_waste_avoided, color = recycling_type)) + 
  labs(title = "Effect of recycling on virgin waste", 
       y = "Virgin waste avoided", 
       color = "Recycling type", 
       caption = "Virgin waste avoided per unit of total waste generation") +
  theme(text = element_text(size = 18))

################# avoidance ###########################
avoidance <- read_csv("Result_data/avoidance.csv") %>%
  mutate(avoidance = avoidance / 10, 
         recycling_yield = sorting * reprocessing) %>%
  mutate(difference = recycling_yield - avoidance) %>%
  filter(country != "LAO") %>% #something wonky going on here...
  left_join(select(filled, country, waste_generation, trade_fraction)) 


mean(avoidance$difference)
sd(avoidance$difference)

ave_avoidance <- avoidance %>%
  group_by(country, type) %>%
  summarise(avoidance_mean = mean(avoidance), avoidance_sd = sd(avoidance),
            mean_difference = mean(difference)) %>%
  left_join(select(filled, country, waste_generation, trade_fraction)) %>%
  mutate(proportion_waste = waste_generation / sum(twg))

mech_avoidance <- filter(ave_avoidance, type == "Mechanical")
chem_avoidance <- filter(ave_avoidance, type == "Chemical")



weighted.mean(mech_avoidance$avoidance_mean, mech_avoidance$waste_generation)
weighted.mean(chem_avoidance$avoidance_mean, chem_avoidance$waste_generation)

ggplot(avoidance) + 
  geom_smooth(aes(x = recycling_yield, y = avoidance), color = pal1[5])  + 
  geom_point(aes(x = recycling_yield, y = avoidance), alpha = .1, color = pal1[6], size = 1)  + 
  facet_wrap(facets = "type") +
  #scale_color_brewer(palette = "YlGnBu") + 
  theme_light() + 
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  labs(title = "Avoidance vs. recycling efficiency", y = "Avoided per unit recycled", x = "Overall recycling efficiency")

ggplot(avoidance) + 
  geom_smooth(aes(x = trade_fraction, y = avoidance), color = pal1[5])  + 
  geom_point(aes(x = trade_fraction, y = avoidance), alpha = .1, color = pal1[6], size = 1)  + 
  facet_wrap(facets = "type") +
  #scale_color_brewer(palette = "YlGnBu") + 
  theme_light() + 
  theme(text = element_text(size = 20), legend.position = "bottom") + 
  labs(title = "Avoidance vs. recycling efficiency", y = "Avoided per unit recycled", x = "Overall recycling efficiency")

corrplot(cor(mech_avoidance[, 3:8]))

cor(mech_avoidance$avoidance_mean, mech_avoidance$trade_fraction)
cor(chem_avoidance$avoidance_mean, mech_avoidance$trade_fraction)

cor(avoidance %>% filter(type == "Mechanical") %>% select(avoidance), avoidance %>% filter(type == "Mechanical") %>% select(recycling_yield))
cor(avoidance %>% filter(type == "Chemical") %>% select(avoidance), avoidance %>% filter(type == "Chemical") %>% select(recycling_yield))

cor(avoidance$avoidance, avoidance$recycling_yield)
cor(avoidance$avoidance, avoidance$trade_fraction)
m <- lm(avoidance ~ recycling_yield + trade_fraction, avoidance)
summary(m)

################# VWG/TWG by r ########################
vwg_prop_vsr <- read_csv("Result_data/vwg_prop_pairwise.csv") %>%
  arrange(type)


#diff <- vwg_prop_vsr[vwg_prop_vsr$type == "Mechanical", "vwg_prop"] - vwg_prop_vsr[vwg_prop_vsr$type == "Chemical", "vwg_prop"]
len = length(vwg_prop_vsr[vwg_prop_vsr$type == "Mechanical",]$iter)
#vwg_prop_vsr <- mutate(vwg_prop_vsr, diff = lag(vwg_prop, len) - vwg_prop)

#diff_color = pal[6]

vwg <- mutate(vwg_prop_vsr, virgin_content = (virgin_content - lag(virgin_content, len)), type = "Difference") %>%
  filter(!is.na(virgin_content)) %>%
  rbind(vwg_prop_vsr)
  
vwg$type <- factor(vwg$type, levels = c("Chemical", "Mechanical", "Difference"))

p <- ggplot(vwg) + 
     geom_point(aes(x = r, y = virgin_content, color = type), size = 1.5)  + 
     #geom_smooth(aes(x = r, y = virgin_content, color = type), se = F, size = 1)  + 
     scale_color_brewer(palette = "YlGnBu") + 
     theme_tufte() + 
     theme(text = element_text(size = 20), legend.position = "bottom") + 
     labs(title = "Virgin content by end-of-life recycling rate", y = "Virgin content", x = "r")


Cairo(width = 2800, height = 2000, file="Figures/chem_benefit.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()




################# predicting VWG with r ######################
predictions <- read_csv("Result_data/vwg_prediction.csv") %>% 
  left_join(select(filled, country, imports, exports, trade_fraction, C1_rate)) %>%
  mutate(prop_actual = vwg / twg,
         prop_predicted = vwg_predict / twg, 
         prop_residual = prop_actual - prop_predicted, 
         exp_frac = (exports - imports) / (twg),
         exp_frac_normal = (exp_frac - min(exp_frac)) /(max(exp_frac) - min(exp_frac)) )  #%>%
  filter(vwg > 0)


mean(predictions$prop_residual)
sd(predictions$prop_residual)



ggplot(predictions) + 
  geom_point(aes(x = r, y = prop_actual), color = pal1[3], alpha = 0.7, shape = 1) + 
  geom_point(aes(x = r, y = prop_predicted), color = pal1[6]) + 
  facet_wrap("type") +
  labs(title = "Predicting VWG with r values", y = "VWG/TWG") + 
  theme_tufte() +
  theme(text = element_text(size = 20), legend.position = "none") 
  
ggplot(predictions) + 
  geom_density(aes(x = prop_residual), color = pal1[6]) + 
  facet_wrap("type") +
labs(title = "Predicting VWG with r values", y = "VWG/TWG") + 
  theme_tufte() +
  theme(text = element_text(size = 20), legend.position = "none") 

p <- ggplot(predictions) + 
  geom_point(aes(x = r, y = prop_actual, color = trade_fraction, size = imports/twg), alpha = 0.3)  +
  geom_point(aes(x = r, y = prop_predicted), color = pal1[6]) + 
  scale_color_distiller(palette = "RdYlBu") +
  facet_wrap("type") +
  labs(title = "Predicting VWG with r values", y = "VWG/TWG", color = "Export fraction", size = "Import fraction") + 
  theme_tufte() +
  theme(text = element_text(size = 36), panel.spacing.x = unit(16, "mm")) 



Cairo(width = 6000, height = 2800, file="Figures/pred_error_by_trade.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()



m1 <- lm(prop_actual ~ r, predictions)
summary(m1)

m2 <- lm(prop_actual ~ r, predictions %>% filter(trade_fraction < 0.5, vwg > 0))
summary(m2)

m3 <- lm(prop_actual ~ r, predictions %>% filter(trade_fraction < 0.2, vwg > 0))
summary(m3)


pred_mech_chem_diff <- predictions %>%
  select(iter, country, type, r, prop_actual, prop_predicted) %>%
  pivot_wider(id_cols = c("iter", "country", "r"), names_from = type, values_from = c("prop_actual", "prop_predicted")) %>%
  mutate(chem_actual_benefit = prop_actual_Mechanical - prop_actual_Chemical,
         chem_predicted_benefit = prop_predicted_Mechanical - prop_predicted_Chemical) %>%
  select(-prop_actual_Mechanical, -prop_actual_Chemical, -prop_predicted_Mechanical, -prop_predicted_Chemical) %>%
  pivot_longer(cols = c("chem_actual_benefit", "chem_predicted_benefit"), names_to = "type", values_to = "benefit")
pred_mech_chem_diff[pred_mech_chem_diff$type == "chem_actual_benefit", "type"] <- "Empirical benefit"
pred_mech_chem_diff[pred_mech_chem_diff$type == "chem_predicted_benefit", "type"] <- "Theoretical benefit"

p <- ggplot(pred_mech_chem_diff) + geom_point(aes(x = r, y = benefit, color = type)) +
  geom_smooth(data = filter(pred_mech_chem_diff, type == "Empirical benefit"), aes(x = r, y = benefit)) +
  scale_color_brewer(palette = "YlGnBu") + 
  labs(title = "Benefit of chemical recycling", y = "Extra VWG reduction", color = "Type") + 
  theme_tufte() + 
  theme(text = element_text(size = 36), panel.spacing.x = unit(16, "mm")) 



Cairo(width = 6000, height = 2800, file="Figures/theoretical_empirical_chem_mechan.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(p)
dev.off()






####################### sensitivity of VWG reduction to displacement rate ##############################
intervals = 100
coeffs <- data.frame(d = seq(1/intervals, 1, 1/intervals),
                     coefficient = numeric(intervals))
for(ds in coeffs$d){
  # data <- data.frame(r1_b = .8,
  #                    improvement = seq(1 + 1/intervals, 4, 1/intervals),
  #                    r2_d = ds) %>%
  #   mutate(r2_b = r1_b * r2_d,
  #          r1_i = r1_b * improvement, 
  #          r2_i = r2_b * improvement,
  #          vc1b = 1/(1 + r1_b + r1_b^2),
  #          vc1i = 1/(1 + r1_i + r1_i^2),
  #          vc2b = 1/(1 + r2_b + r2_b^2),
  #          vc2i = 1/(1 + r2_i + r2_i^2),
  #          vc1p = (vc1b - vc1i)/vc1b,
  #          vc2p = (vc2b - vc2i)/vc2b,
  #          imp_change = (vc2i-vc1i)/vc1i)
  
  data <- data.frame(r1_b = seq(1/intervals, 1, 1/intervals),
                     d = ds) %>%
    mutate(r1_d = r1_b * d,
           vc1b = 1/(1 + r1_b + r1_b^2),
           vc1d = 1/(1 + r1_d + r1_d^2),
           ratio = vc1d/vc1b) %>%
    mutate(vc1d_predicted = vc1b * ratio)
  
           
         
  
  ggplot(data) + geom_point(aes(x = r1_b, y = ratio))
 # m <- lm(imp_change ~ exp(vc1p), data)$coefficients 
  m <- lm(imp_change ~ improvement, data)$coefficients 
  coeffs[coeffs$d == ds, 2] = m[2]
}
ggplot(coeffs) + geom_point(aes(x = d, y = coefficient))
lm((1/d) ~ coefficient, data = coeffs) %>% summary()

ggplot(data) + geom_point(aes(x = vc1p, y = imp_change))
ggplot(data) + geom_point(aes(x = exp(vc1p), y = imp_change))

lm(imp_change ~ improvement, data) %>% summary()



####################### functions ########################
baseline_fill_mean <- function(year, trade_probs, rates_unfilled){
  export_totals <- trade_probs %>%
    group_by(from) %>%
    summarise(exports = sum(quantity))
  import_totals <- trade_probs %>%
    group_by(to) %>%
    summarise(imports = sum(quantity))
  rate_data_iter <- rates_unfilled %>% left_join(export_totals, by = c("country" = "from")) %>%
    left_join(import_totals, by = c("country" = "to"))
  rate_data_iter[is.na(rate_data_iter$exports), "exports"] <- 0
  rate_data_iter[is.na(rate_data_iter$imports), "imports"] <- 0
  rate_data_iter[rate_data_iter$country == "EU", "mismanagement_rate"] = eu_mismanagement[eu_mismanagement$year == year, "r"]
  rate_data_iter$mismanagement_rate <- rate_data_iter$mismanagement_rate - 0.02 #removing the 2% addition for littering so it isn't wrongfully scaled up on line 652, after which point it gets added back in.
  
  sorting_y <- runif(1, .59, .83) # EU MFA
  recycling_y <- runif(1, .70, .85)
  Imp_misman <- runif(1, .25, .75)
  I_C1_rate <- runif(1, 0.75, 0.85)
  C1_rate_unknown <- 0.1
  
  
  rates_iter_base <- rate_data_iter %>%
    mutate(total_managed = waste_generation + imports - exports)
  rates_iter_base$mechanical_yield_rate <- sorting_y * recycling_y #sorting efficiency times mechanical process yield
  rates_iter_base$chemical_yield_rate <- sorting_y * recycling_y #sorting efficiency times chemical process yield
  rates_iter_base$C1_rate[is.na(rates_iter_base$C1_rate)] <- C1_rate_unknown
  rates_iter_base$C2_rate <- rates_iter_base$C1_rate
  rates_iter_base[rates_iter_base$mismanagement_rate >= 0.2, "I_mismanagement"] <- Imp_misman
  rates_iter_base[rates_iter_base$mismanagement_rate < 0.2, "I_mismanagement"] <- 0
  rates_iter_base$trade_fraction <- rates_iter_base$exports / (rates_iter_base$waste_generation * rates_iter_base$C1_rate)
  rates_iter_base$trade_fraction[rates_iter_base$trade_fraction > 0.9] <- 0.9
  rates_iter_base$I_C1 <- I_C1_rate
  rates_iter_base$mismanagement_rate <- rates_iter_base$mismanagement_rate / (1 - rates_iter_base$C1_rate) #mismanagement rates give the probability that waste is mismanaged. This term conditions that probability on the event that the waste is not recycled, which is necessary because flows to sink states all involve linear disposal of waste.
  rates_iter_base$mismanagement_rate <- rates_iter_base$mismanagement_rate + 0.02
  rates_iter_base$mismanagement_rate[rates_iter_base$mismanagement_rate > 1] <- 1
  rates_iter_base <- mutate(rates_iter_base, recycling_capacity = (imports * I_C1) + (waste_generation * C1_rate),
                            modeled_exp = waste_generation*C1_rate*trade_fraction)
  
  
  return(rates_iter_base)
}


