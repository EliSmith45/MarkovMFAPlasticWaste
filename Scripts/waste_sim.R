library(tidyverse)
library(microbenchmark)
library(Matrix.utils)
library(data.table)
library(ggthemes)
library(RColorBrewer)
# library(hrbrthemes)
# library(ggridges)
options(dplyr.summarise.inform = FALSE)

############################ read in rate data ###############################

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



rate_data[rate_data$country == "IND", "C1_rate"] = 0.1
eu_mismanagement <- read_csv("Data/eu_mismanagement_rates.csv")
countries <- rate_data$country

twg <- as.matrix(rate_data[order(rate_data$country),"waste_generation"])
rownames(twg) <- rate_data[,"country"]
twg


#pre-populates a list of transitions to make assigning their respective probabilities much faster
processes <- c("G1","G2", "G3", "C1", "C2", "I", "Managed", "Mismanaged")
transitions_list <- data.frame(from = rep(processes[1:6], times = 8),
                               to = rep(processes, each = 6),
                               probability = numeric(48), 
                               country = rep(countries, each = 48))

trade_year = 2019 # which year's trade data to default to
trade_flow_data <- read_csv(paste("Data/trade_clean/trade_probs_corrected", trade_year, ".csv", sep = ""))

#uncorrected trade data
# trade_flow_data <- read_csv(paste("Data/trade_clean/trade_probs", trade_year, ".csv", sep = "")) %>%
#   filter(from %in% countries, to %in% countries) %>%
#   left_join(select(rate_data, country, mismanagement_rate), by = c("from" = "country")) %>%
#   rename(exporter_mismanagement = mismanagement_rate) %>%
#   left_join(select(rate_data, country, mismanagement_rate), by = c("to" = "country")) %>%
#   rename(importer_mismanagement = mismanagement_rate) %>%
#   filter(from != "NAM") %>% #data contains zero flows for NAM
#   as.data.frame()

trade_flow_data[trade_flow_data$from == "EU", "exporter_mismanagement"] = eu_mismanagement[eu_mismanagement$year == 2019, "r"]
trade_flow_data[trade_flow_data$to == "EU", "importer_mismanagement"] = .05741
trade_flow_data <- select(trade_flow_data, -trade_fraction, -old_trade, -percent_change)
#trade_good <- trade_flow_data[trade_flow_data$exporter_mismanagement >= trade_flow_data$importer_mismanagement, c(-4, -5)]


############################ scenarios ###############################

### Import recycling correction factor info
f <- baseline_fill(2019, trade_flow_data, rate_data)$mat
low_vwg <- f %>% filter(max_I_C1 < 0.8) %>%
  select(country, waste_generation, imports, re, max_I_C1) %>%
  mutate(lost_imp_rec = 0.8 - max_I_C1)
sum(low_vwg$waste_generation) / sum(f$waste_generation)

low_vwg %>% select(imports) %>% sum() / sum(f$imports)
sum(low_vwg$waste_generation) / sum(f$waste_generation)
sum(low_vwg$imports) / sum(f$imports)

(sum(f$imports * 0.8) - sum(low_vwg$imports * (0.8 - low_vwg$max_I_C1))) / sum(f$imports * 0.8)




 ### fix trade data
# old_trade <- left_join(trade_flow_data, select(f, country, trade_fraction, old_trade), by = c("from" = "country")) %>%
#   mutate(percent_change = (old_trade - trade_fraction)/old_trade)
# corrected_trade <- old_trade %>%
#   mutate(quantity = (quantity * (1 - percent_change)),
#          exports = (exports * (1 - percent_change)))
# 
# new_trade_total <-corrected_trade %>% select(from, exports) %>% unique() %>% select(exports) %>% sum()
# old_trade_total <-old_trade %>% select(from, exports) %>% unique() %>% select(exports) %>% sum()
# (old_trade_total - new_trade_total) / old_trade_total
# old_trade_total/sum(f$waste_generation)
# write_csv(corrected_trade, "Data/trade_clean/trade_probs_corrected2019.csv")

# These lines calculate the results for the listed scenarios, with [iters] number of iterations for each scenario.
# Results are written to [filename].csv every 500 iterations
# recycling_results <- run_scenarios_pairwise(scenarios = c("increase sorting & yield",
#                                                           "increase mech, chem, yield",
#                                                           "50%, chemical",
#                                                           "50%, mechanical",
#                                                           "baseline"),
#                                             iters = 10000, 
#                                             filename = "Result_data/pairwise_recycling.csv")
# 
# trade_results <- run_scenarios_pairwise(scenarios = c("no trade, landfill, use capacity",
#                                                       "no trade, recycle, use capacity",
#                                                       "no trade, landfill",
#                                                       "no trade, recycle",
#                                                       "baseline"),
#                                         iters = 10000,
#                                         filename = "Result_data/pairwise_trade_collec.csv")


scenario_results <- run_scenarios_pairwise(scenarios = c("no trade, landfill, use capacity",
                                                         "no trade, recycle, use capacity",
                                                         "no trade, landfill",
                                                         "no trade, recycle",
                                                         "baseline",
                                                         "20% import recycling max",
                                                         "increase sorting",
                                                         "increase sorting & yield",
                                                         "increase mech, chem, yield",
                                                         "50%, chemical",
                                                         "50%, mechanical"),
                                          iters = 10000,
                                          global_filename = "Result_data/all_scenarios_corrected_trade.csv",
                                          country_filename = "Result_data/pairwise_corrected_trade_scenarios_cty.csv")

global <- scenario_results$global
cty <- scenario_results$country
write_csv(global, "Result_data/all_scenarios_corrected_trade.csv")
write_csv(cty, "Result_data/pairwise_corrected_trade_scenarios_cty.csv")



##################### Avoidance ########################


#country level avoidance rate
# total_gen = sum(twg)
# extra_rec = 10
# iters = 50
# 
# avoidance <- data.frame(iter = rep(rep(1:iters, each = length(countries)), times = 2),
#                         country = rep(countries, times = 2 * length(iters)),
#                         avoidance = numeric(2 * iters * length(countries)),
#                         sorting = numeric(2 * iters * length(countries)),
#                         reprocessing = numeric(2 * iters * length(countries)),
#                         type = rep(c("Mechanical", "Chemical"), each = (iters * length(countries))))
#                          
# for(country in countries){
#   print(country) 
#   
#   for(i in 1:iters){
#     filled <- baseline_fill(year = trade_year, trade_probs = trade_flow_data, rates_unfilled = rate_data)
#     rates_iter_base <- filled$mat
#     params <- filled$params
#     
#     v0 <- make_model_detailed(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
#     a0 = sum(v0)
#     
#     rates_iter_base[rates_iter_base$country == country, "fraction_recycling_chemical"] <- 1
#     v1 <- make_model_detailed(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
#     a1 = sum(v1)
#     
#     rates_iter_base[rates_iter_base$country == country, "C1_rate"] = rates_iter_base[rates_iter_base$country == country, "C1_rate"] + (extra_rec / v0[rownames(v0) == country])
#     v2 <- make_model_detailed(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
#     a2 = sum(v2)
#     
#     rates_iter_base[rates_iter_base$country == country, "fraction_recycling_chemical"] <- 0
#     v3 <- make_model_detailed(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
#     a3 = sum(v3)
#     
#     avoidance[avoidance$iter == i & avoidance$country == country & avoidance$type == "Mechanical", 3:5] = c(a0 - a3, params[1], params[2]) #avoidance rate, sorting yield, reprocessing yield
#     avoidance[avoidance$iter == i & avoidance$country == country & avoidance$type == "Chemical", 3:5] = c(a1 - a2, params[1], params[2]) #avoidance rate, sorting yield, reprocessing yield
#     
#     print(i)
#   }
# }
# 
# write_csv(avoidance, "Result_data/avoidance_pairwise.csv")
# 



#chem benefit over r
iters = 500
vwg_prop_vsr <- data.frame(iter = rep(1:iters, times = 2),
                           virgin_content = numeric(2 * iters),
                           rec_efficiency = numeric(2 * iters),
                           rec_rate = numeric(2 * iters),
                           type = rep(c("Mechanical", "Chemical"), each = iters))
for(i in 1:iters){
  print(i)

  filled <- baseline_fill(year = trade_year, trade_probs = trade_flow_data, rates_unfilled = rate_data)
  rates_iter_base <- filled$mat
  params <- filled$params


  #bounds are chosen as such to achieve a more uniform distribution of end-of-life recycling rates
  refficiency = sqrt(rbeta(1, 0.5, 0.5))
  recycling = sqrt(rbeta(1, 1, 0.5))

  rates_iter_base$mechanical_yield_rate <- refficiency
  rates_iter_base$chemical_yield_rate <- refficiency
  rates_iter_base$C1_rate <- recycling
  rates_iter_base$C2_rate <- recycling

  v0 <- make_model_cty_vwg(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
  res0 <- c(sum(v0) / total_gen, refficiency, recycling)

  rates_iter_base$fraction_recycling_chemical <- 1
  v1 <- make_model_cty_vwg(rates_iter = rates_iter_base, generation_total = twg, trade = trade_flow_data)
  res1 <- c(sum(v1) / total_gen, refficiency, recycling)

  vwg_prop_vsr[vwg_prop_vsr$iter == i & vwg_prop_vsr$type == "Mechanical", 2:4] = res0
  vwg_prop_vsr[vwg_prop_vsr$iter == i & vwg_prop_vsr$type == "Chemical", 2:4] = res1

  if(i %% 100 == 0){
    write_csv(vwg_prop_vsr, "Result_data/vwg_prop_pairwise.csv")
  }

}


vwg_prop_vsr <- vwg_prop_vsr %>%
  mutate(r = rec_efficiency * rec_rate,
         vc_theoretical = ifelse(type == "Mechanical", 1 / (1 + r + r^2), (1 - r)),
         residual = virgin_content - vc_theoretical)
write_csv(vwg_prop_vsr, "Result_data/vwg_prop_pairwise.csv")

############################ impact of trade over time ###############################


# trade_impact_over_time <- data.frame(virgin_waste = double(),
#                                      total_mismanaged = double(),
#                                      total_waste = double(),
#                                      year = numeric())
# 
# 
# for(y in 1995:2019){
#   print(y)
#   trade_flow_data <- read_csv(paste("Data/trade_clean/trade_probs", y, ".csv", sep = "")) %>%
#     filter(from %in% countries, to %in% countries) %>%
#     left_join(select(rate_data, country, mismanagement_rate), by = c("from" = "country")) %>%
#     rename(exporter_mismanagement = mismanagement_rate) %>%
#     left_join(select(rate_data, country, mismanagement_rate), by = c("to" = "country")) %>%
#     rename(importer_mismanagement = mismanagement_rate)
#   
#   for(i in 1:100){
#     rates_iter_base <- baseline_fill(year = trade_year, trade_probs = trade_flow_data, rates_unfilled = rate_data)
#     vals = make_model(rates_iter = rates_iter_base, trade = trade_probs)
#     res <- data.frame(virgin_waste = vals[2],
#                       total_mismanaged = vals[1],
#                       total_waste = vals[3], 
#                       year = y)
#     trade_impact_over_time <- rbind(trade_impact_over_time, res)
#     print(i)
#   }
# }
# 
# 
# write_csv(trade_impact_over_time, "Result_data/trade_impact.csv")
# 


######################### functions #########################

baseline_fill <- function(year, trade_probs, rates_unfilled){
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
  C1_rate_unknown <- runif(1, 0.05, 0.15)
  
  
  rates_iter_base <- rate_data_iter %>%
    mutate(total_managed = waste_generation + imports - exports)
  rates_iter_base$mechanical_yield_rate <- sorting_y * recycling_y #sorting efficiency times mechanical process yield
  rates_iter_base$chemical_yield_rate <- sorting_y * recycling_y #sorting efficiency times chemical process yield
  rates_iter_base$C1_rate[is.na(rates_iter_base$C1_rate)] <- C1_rate_unknown
  rates_iter_base$C2_rate <- rates_iter_base$C1_rate
  rates_iter_base[rates_iter_base$mismanagement_rate >= 0.2, "I_mismanagement"] <- Imp_misman
  rates_iter_base[rates_iter_base$mismanagement_rate < 0.2, "I_mismanagement"] <- 0
  rates_iter_base$trade_fraction <- rates_iter_base$exports / (rates_iter_base$waste_generation * rates_iter_base$C1_rate)
  # rates_iter_base$old_trade <- rates_iter_base$trade_fraction
  rates_iter_base$trade_fraction[rates_iter_base$trade_fraction > 0.9] <- 0.9
  rates_iter_base <- mutate(rates_iter_base, 
                            re = C1_rate * mechanical_yield_rate * (1 - trade_fraction),
                            max_I_C1 = 0.1 * waste_generation / (imports * (1 + re)))
  rates_iter_base$I_C1 <- pmin(I_C1_rate, rates_iter_base$max_I_C1)
  rates_iter_base$mismanagement_rate <- rates_iter_base$mismanagement_rate / (1 - rates_iter_base$C1_rate) #mismanagement rates give the probability that waste is mismanaged. This term conditions that probability on the event that the waste is not recycled, which is necessary because flows to sink states all involve linear disposal of waste.
  rates_iter_base$mismanagement_rate <- rates_iter_base$mismanagement_rate + 0.02
  rates_iter_base$mismanagement_rate[rates_iter_base$mismanagement_rate > 1] <- 1
  rates_iter_base <- mutate(rates_iter_base, recycling_capacity = (imports * I_C1) + (waste_generation * C1_rate),
                            modeled_exp = waste_generation*C1_rate*trade_fraction)
  
  return(list(mat = rates_iter_base, params = c(sorting_y, recycling_y, Imp_misman, I_C1_rate, C1_rate_unknown)))
}

make_dtmc <- function(transitions, trade_data){
  all_transitions <- as.data.table(transitions)

  all_transitions[from == "G1" & to == "Mismanaged", probability := mismanagement_rate * (1-C1_rate)]
  all_transitions[from == "G1" & to == "Managed", probability := (1 - mismanagement_rate) * (1-C1_rate)]
  all_transitions[from == "G1" & to == "C1", probability := C1_rate]
  
  all_transitions[from == "G2" & to == "Mismanaged", probability := mismanagement_rate * (1-C1_rate)]
  all_transitions[from == "G2" & to == "Managed", probability := (1 - mismanagement_rate) * (1-C1_rate)]
  all_transitions[from == "G2" & to == "C2", probability := C1_rate]
  
  all_transitions[from == "G3" & to == "Mismanaged", probability := mismanagement_rate]
  all_transitions[from == "G3" & to == "Managed", probability := (1 - mismanagement_rate)]

  all_transitions[from == "C1" & to == "G1", probability := fraction_recycling_chemical * chemical_yield_rate * (1 - trade_fraction)] 
  all_transitions[from == "C1" & to == "G2", probability := (1 - fraction_recycling_chemical) * mechanical_yield_rate * (1 - trade_fraction)] 
  all_transitions[from == "C1" & to == "Mismanaged", probability := (1 - trade_fraction) * mismanagement_rate  * ((fraction_recycling_chemical * (1 - chemical_yield_rate)) + ((1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate)))] 
  all_transitions[from == "C1" & to == "Managed", probability := (1 - trade_fraction) * (1 - mismanagement_rate)  * ((fraction_recycling_chemical * (1 - chemical_yield_rate)) + ((1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate)))] 
  
  all_transitions[from == "C2" & to == "G1", 3] <- all_transitions[from == "C1" & to == "G1", 3]
  all_transitions[from == "C2" & to == "G3", 3] <- all_transitions[from == "C1" & to == "G2", 3]
  all_transitions[from == "C2" & to == "Managed", 3] <- all_transitions[from == "C1" & to == "Managed", 3]
  all_transitions[from == "C2" & to == "Mismanaged", 3] <- all_transitions[from == "C1" & to == "Mismanaged", 3]
  
  all_transitions[from == "I" & to == "G1", probability := I_C1 * fraction_recycling_chemical * chemical_yield_rate]
  all_transitions[from == "I" & to == "G2", probability := I_C1 * (1 - fraction_recycling_chemical) * mechanical_yield_rate]
  
  all_transitions[from == "I" & to == "Mismanaged", probability := (I_mismanagement) * ((1 - I_C1) + (I_C1 * fraction_recycling_chemical * (1 - chemical_yield_rate)) + I_C1 * (1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate))] #(amount that didn't get recycled + amount lost during chemical recycling + amount lost during mechanical recycling) * mismanagement rate
  all_transitions[from == "I" & to == "Managed", probability := (1 - I_mismanagement) * ((1 - I_C1) + (I_C1 * fraction_recycling_chemical * (1 - chemical_yield_rate)) + I_C1 * (1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate))] 
  
  
  
  
  
  # concatenates processing state and country code in from and to columns
  all_transitions[!(from %in% c("Managed", "Mismanaged")), from := paste0(from, country)
                  ][!(to %in% c("Managed", "Mismanaged")), to := paste0(to, country)]
  probs <- all_transitions[, 1:3]
  probs <- rbindlist(list(probs, list("Mismanaged", "Mismanaged", 1), list("Managed", "Managed", 1)))
  
  fracs <- unique(all_transitions[,c("country", "trade_fraction")]) #just gets each exporter's trade fraction
  trade_data <- merge(x = trade_data, 
                             y = fracs, 
                             by.x = "from", 
                             by.y = "country",
                             all.x = TRUE,
                             all.y = FALSE,
                             fill.y = 0) #left outer join trade fractions for exporters
  
  
  trade_data <- trade_data[rep(seq_len(nrow(trade_data)), 2),] #effectively is rowbinding the data set to itself cause all flows come from both C1 and C2
  trade_data[,':=' (probability = probability * trade_fraction,
                    from = paste0(c(rep("C1", times = 0.5*length(trade_data[,from])), 
                               rep("C2", times = 0.5*length(trade_data[,from]))), from),
                    to = paste0(rep("I", times = length(trade_data[,from])), to))]
  
  probs <- rbindlist(list(probs, trade_data[, c("from", "to", "probability")])) #attach trade transition probabilities to full list
  
  # create the transition matrix from the data frame
  P <- dcast(probs, from ~ to, value.var = "probability", fill = 0)
  P <- as.matrix(P[, -1], rownames.value = P[, from])
  
  # print(isTRUE(all.equal(as.numeric(apply(P, 1, sum)), rep(1, times = dim(P)[1]))))
  # min(apply(P, 1, sum))
  # max(apply(P, 1, sum))
  # asdf <- apply(P, 1, sum) %>% as.data.frame()
  # asdf <- asdf %>%
  #   mutate(state = row.names(asdf)) %>%
  #   rename(total_prob = ".") %>%
  #   filter(total_prob != 1)
  
  
  return(P)
}
make_model <- function(rates_iter, generation_total, trade){  
  rates_iter <- merge.Matrix(transitions_list, rates_iter, by.x = transitions_list[, "country"], by.y = rates_iter[, "country"])[, -c(5, 7)]
  P <- make_dtmc(transitions = rates_iter, trade_data = as.data.table(trade))
  
  Q <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), !(colnames(P) %in% c("Managed", "Mismanaged"))]
  I <- diag(nrow(Q))
  R <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), (colnames(P) == "Mismanaged")]
  Q <- Q[order(rownames(Q)), order(colnames(Q))]
  N <- solve(I - Q)
  
  
  NG <- N[substr(rownames(N), 1, 2) == "G1", substr(colnames(N), 1, 1) == "G"] 
  rownames(NG) <- substr(rownames(NG), 3, 5)
  #NG$source_country <- rownames(NG)
  NG <- NG[order(rownames(NG)), order(colnames(NG), substr(colnames(NG), 3, 5))]
  G_throughput <- NG[, 1:nrow(NG)] + NG[, (nrow(NG) + 1):(2*nrow(NG))] + NG[, (2*nrow(NG) + 1):(3*nrow(NG))]
  
  
  virgin_waste_fast <- solve(t(G_throughput), generation_total)
  abs_probs_fast <- N %*% R
  mismanaged <- abs_probs_fast[substr(rownames(abs_probs_fast), 1, 2) == "G1",] * virgin_waste_fast
  
  # res_summary <- c(sum(res_detailed$total_mismanaged), sum(res_detailed$virgin_waste), sum(res_detailed$waste_generation))
  # result_data <- list(detailed = res_detailed, summary = res_summary, fundamental_matrix = N)  
  # 
  
  return(c(sum(mismanaged), sum(virgin_waste_fast), sum(generation_total)))
  
} 


make_model_direct_disposal <- function(rates_iter, generation_total, trade){  
  rates_iter <- merge.Matrix(transitions_list, rates_iter, by.x = transitions_list[, "country"], by.y = rates_iter[, "country"])[, -c(5, 7)]
  P <- make_dtmc(transitions = rates_iter, trade_data = as.data.table(trade))
  
  Q <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), !(colnames(P) %in% c("Managed", "Mismanaged"))]
  I <- diag(nrow(Q))
  R <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), (colnames(P) %in% c("Managed", "Mismanaged"))]
  Q <- Q[order(rownames(Q)), order(colnames(Q))]
  N <- solve(I - Q)
  
  
  NG <- N[substr(rownames(N), 1, 2) == "G1", substr(colnames(N), 1, 1) == "G"] 
  rownames(NG) <- substr(rownames(NG), 3, 5)
  #NG$source_country <- rownames(NG)
  NG <- NG[order(rownames(NG)), order(colnames(NG), substr(colnames(NG), 3, 5))]
  G_throughput <- NG[, 1:nrow(NG)] + NG[, (nrow(NG) + 1):(2*nrow(NG))] + NG[, (2*nrow(NG) + 1):(3*nrow(NG))]
  
  
  virgin_waste_fast <- solve(t(G_throughput), generation_total)
  abs_probs_fast <- N %*% R[,2]
  mismanaged <- abs_probs_fast[substr(rownames(abs_probs_fast), 1, 2) == "G1",] * virgin_waste_fast
  
  # res_summary <- c(sum(res_detailed$total_mismanaged), sum(res_detailed$virgin_waste), sum(res_detailed$waste_generation))
  # result_data <- list(detailed = res_detailed, summary = res_summary, fundamental_matrix = N)  
  
  inputs <- setNames(nm = rownames(N), object = c(rep(0, length(countries) * 2), virgin_waste_fast, rep(0, length(countries) * 3)))
  throughput <- inputs %*% N
  disposalRate <- as.data.table(list(state = colnames(throughput),
                                     country = ifelse(substr(colnames(throughput), 1, 1) == "I", 
                                                      substr(colnames(throughput), 2, 4),
                                                      substr(colnames(throughput), 3, 5)),
                                     disposed = t(throughput) * apply(R, 1, sum)))
  disposalRate <- disposalRate[, .(disposed = sum(disposed)), by = country]
  
  return(list(global = c(sum(mismanaged), sum(virgin_waste_fast), sum(generation_total)),
              country = disposalRate))
  
} 

make_model_cty_vwg <- function(rates_iter, generation_total, trade){  
  rates_iter <- merge.Matrix(transitions_list, rates_iter, by.x = transitions_list[, "country"], by.y = rates_iter[, "country"])[, -c(5, 7)]
  P <- make_dtmc(transitions = rates_iter, trade_data = as.data.table(trade))
  
  Q <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), !(colnames(P) %in% c("Managed", "Mismanaged"))]
  I <- diag(nrow(Q))
  R <- P[!(rownames(P) %in% c("Managed", "Mismanaged")), (colnames(P) == "Mismanaged")]
  Q <- Q[order(rownames(Q)), order(colnames(Q))]
  N <- solve(I - Q)
  
  
  NG <- N[substr(rownames(N), 1, 2) == "G1", substr(colnames(N), 1, 1) == "G"] 
  NG <- as.data.frame(NG) 
  rownames(NG) <- substr(rownames(NG), 3, 5)
  #NG$source_country <- rownames(NG)
  NG <- NG[order(rownames(NG)), order(colnames(NG), substr(colnames(NG), 3, 5))]
  G_throughput <- NG[, 1:nrow(NG)] + NG[, (nrow(NG) + 1):(2*nrow(NG))] + NG[, (2*nrow(NG) + 1):(3*nrow(NG))]
  
  
  virgin_waste_fast <- as.matrix(solve(t(G_throughput), generation_total))
  rownames(virgin_waste_fast) <- rownames(generation_total)
  # abs_probs_fast <- N %*% R
  # abs_probs_fast <- abs_probs_fast[substr(rownames(abs_probs_fast), 1, 2) == "G1",]
  # mismanaged <- abs_probs_fast * virgin_waste_fast
  # 
  # res_summary <- c(sum(res_detailed$total_mismanaged), sum(res_detailed$virgin_waste), sum(res_detailed$waste_generation))
  # result_data <- list(detailed = res_detailed, summary = res_summary, fundamental_matrix = N)  
  # 
  return(virgin_waste_fast)
  
} 

run_scenarios_pairwise <- function(scenarios, iters, global_filename, country_filename){
  runs = length(scenarios) * iters
  results <- data.frame(iter = rep(1:iters, times = length(scenarios)),
                        total_mismanaged = double(runs),
                        virgin_waste = double(runs),
                        total_waste = double(runs),
                        sorting_yield = double(runs), 
                        recycling_yield = double(runs),
                        I_mismanaged = double(runs), 
                        I_recycled = double(runs),
                        recycling_rate = double(runs),
                        scenario = rep(scenarios, each = iters))
  results$scenario <- as.character(results$scenario)
  
  cty_level_scenarios = c("no trade, landfill, use capacity",
                          "no trade, recycle, use capacity",
                          "no trade, landfill",
                          "no trade, recycle",
                          "baseline", 
                          "20% import recycling max")
  run_detailed <- scenarios[scenarios %in% cty_level_scenarios]
  
  cty_level <- data.frame(iter = rep(1:iters, times = length(run_detailed), each = length(countries)),
                          scenario = rep(run_detailed, each = length(countries) * iters),
                          country = rep(countries, times = length(run_detailed) * iters),
                          disposed = double(length(run_detailed) * iters * length(countries)))
  
  cty_level$scenario <- as.character(cty_level$scenario)
  cty_level$country <- as.character(cty_level$country)
  
  
  for(i in 1:iters){ #iterate through vector of scenarios
    print(i)
    initialized <- baseline_fill(year = trade_year, trade_probs = trade_flow_data, rates_unfilled = rate_data)
    if(i %% 500 == 0){
      write_csv(results, global_filename)
      write_csv(cty_level, country_filename)
      print(format(Sys.time(), "Results logged on %D at %I:%M:%S %p, %Z"))
    }
    if("baseline" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      
      vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "baseline", 2:4] = vals$global
      results[results$iter == i & results$scenario == "baseline", 5:9] = params
      
      cty_level[cty_level$iter == i & cty_level$scenario == "baseline", 3:4] <- vals$country
      
      
    }
    if("no trade, recycle" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      
      rates_filled$trade_fraction <- 0
      vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "no trade, recycle", 2:4] = vals$global
      results[results$iter == i & results$scenario == "no trade, recycle", 5:9] = params
      
      cty_level[cty_level$iter == i & cty_level$scenario == "no trade, recycle", 3:4] <- vals$country
      
    }
    if("no trade, landfill" %in% scenarios){
      
        rates_filled <- initialized$mat
        params <- initialized$params
        
        rates_filled$C1_rate <- rates_filled$C1_rate * (1 - rates_filled$trade_fraction)
        rates_filled$C2_rate <- rates_filled$C1_rate
        rates_filled$trade_fraction <- 0
        vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
        results[results$iter == i & results$scenario == "no trade, landfill", 2:4] = vals$global
        results[results$iter == i & results$scenario == "no trade, landfill", 5:9] = params
        
        cty_level[cty_level$iter == i & cty_level$scenario == "no trade, landfill", 3:4] <- vals$country
      
    }  
    if("no trade, recycle, use capacity" %in% scenarios){

      rates_filled <- initialized$mat %>% 
        rename(old_C1 = C1_rate) %>%
        mutate(C1_rate = (waste_generation * old_C1 + imports * I_C1)/waste_generation)
      rates_filled$C2_rate <- rates_filled$C1_rate
      rates_filled$trade_fraction <- 0
      rates_filled[rates_filled$mismanagement_rate < 0, "mismanagement_rate"] = 0
      rates_filled[rates_filled$mismanagement_rate < 0, "mismanagement_rate"] = 0
      params <- initialized$params
      
        
      vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "no trade, recycle, use capacity", 2:4] = vals$global
      results[results$iter == i & results$scenario == "no trade, recycle, use capacity", 5:9] = params
      
      cty_level[cty_level$iter == i & cty_level$scenario == "no trade, recycle, use capacity", 3:4] <- vals$country
      
    } 
    if("no trade, landfill, use capacity" %in% scenarios){
        
      rates_filled <- initialized$mat %>% 
        rename(old_C1 = C1_rate) %>%
        mutate(C1_rate = (waste_generation * old_C1 * (1 - trade_fraction) + imports * I_C1 )/waste_generation)
      rates_filled$C2_rate <- rates_filled$C1_rate
      rates_filled$trade_fraction <- 0
      rates_filled[rates_filled$mismanagement_rate < 0, "mismanagement_rate"] = 0
      params <- initialized$params
      
      vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "no trade, landfill, use capacity", 2:4] = vals$global
      results[results$iter == i & results$scenario == "no trade, landfill, use capacity", 5:9] = params
      
      cty_level[cty_level$iter == i & cty_level$scenario == "no trade, landfill, use capacity", 3:4] <- vals$country
    } 
    if("20% import recycling max" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$I_C1 <- pmin(rates_filled$I_C1, 0.2)
      params[4] = 0.2
      vals = make_model_direct_disposal(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "20% import recycling max", 2:4] = vals$global
      results[results$iter == i & results$scenario == "20% import recycling max", 5:9] = params
      
      cty_level[cty_level$iter == i & cty_level$scenario == "20% import recycling max", 3:4] <- vals$country
    }
    if("50%, mechanical" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$trade_fraction <- 0
      rates_filled$C1_rate <- 0.5
      rates_filled$C2_rate <- 0.5
      params[5] = 0.5
      vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "50%, mechanical", 2:4] = vals
      results[results$iter == i & results$scenario == "50%, mechanical", 5:9] = params
      
    }
    
    if("50%, chemical" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$trade_fraction <- 0
      rates_filled$fraction_recycling_chemical <- (0.5 - rates_filled$C1_rate) / 0.5 *.75
      rates_filled$C1_rate <- 0.5
      rates_filled$C2_rate <- 0.5
      params[5] = 0.5
      vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "50%, chemical", 2:4] = vals
      results[results$iter == i & results$scenario == "50%, chemical", 5:9] = params
      
    }
    if("increase mech, chem, yield" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$trade_fraction <- 0
      rates_filled$fraction_recycling_chemical <- (0.5 - rates_filled$C1_rate) / 0.5 *.75
      rates_filled$C1_rate <- 0.5
      rates_filled$mechanical_yield_rate <- 0.9 * 0.9
      rates_filled$chemical_yield_rate <- 0.9 * 0.9
      params <- c(0.9, 0.9, params[3:4], 0.5)
      vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "increase mech, chem, yield", 2:4] = vals
      results[results$iter == i & results$scenario == "increase mech, chem, yield", 5:9] = params
      
    }
    if("increase sorting & yield" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$trade_fraction <- 0
      rates_filled$mechanical_yield_rate <- 0.9 * 0.9
      rates_filled$chemical_yield_rate <- 0.9 * 0.9
      params <- c(0.9, 0.9, params[3:5])
      vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "increase sorting & yield", 2:4] = vals
      results[results$iter == i & results$scenario == "increase sorting & yield", 5:9] = params
      
    }
    if("increase sorting" %in% scenarios){
      
      rates_filled <- initialized$mat
      params <- initialized$params
      rates_filled$trade_fraction <- 0
      rec_yield = runif(1, .70, .85)
      rates_filled$mechanical_yield_rate <- 0.9 * rec_yield
      rates_filled$chemical_yield_rate <- 0.9 * rec_yield
      params <- c(0.9, rec_yield, params[3:5])
      vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
      results[results$iter == i & results$scenario == "increase sorting", 2:4] = vals
      results[results$iter == i & results$scenario == "increase sorting", 5:9] = params
      
    }
    
  }
  
  return(list(global = results,
              country = cty_level))
}


###############old functions###################

# make_model <- function(rates_iter, trade, s){  
  P <- make_dtmc(rates = rates_iter, trade_data = trade, all_transitions = transition_list, scenario = s)
  generation_total = rates_iter %>% 
    select(country, waste_generation) %>% 
    arrange(country)
  absorbing <- c("Managed", "Mismanaged")
  
  Q <- P[!(rownames(P) %in% absorbing), !(colnames(P) %in% absorbing)]
  I <- diag(nrow(Q))
  R <- P[!(rownames(P) %in% absorbing), (colnames(P) %in% absorbing)]
  N <- solve(I - Q)
  
  
  NG <- N[substr(rownames(N), 1, 2) == "G1", substr(colnames(N), 1, 1) == "G"] 
  NG <- as.data.frame(NG) 
  rownames(NG) <- substr(rownames(NG), 3, 5)
  NG <- mutate(NG, source_country = rownames(NG))
  
  NG_pivoted <- pivot_longer(data = NG, cols = 1:(ncol(NG) - 1), names_to = "target_country", values_to = "throughput")
  NG_pivoted$target_country <- substr(NG_pivoted$target_country, 3, 5)
  NGS <- NG_pivoted %>%
    group_by(source_country, target_country) %>%
    summarise(throughput = sum(throughput)) %>% 
    ungroup()
  
  NGS_pivoted <- pivot_wider(NGS, names_from = "target_country", values_from = "throughput")
  row.names(NGS_pivoted) <- NGS_pivoted$source_country
  NGS_pivoted <- select(NGS_pivoted, -source_country)
  NGS_pivoted <- as.matrix(NGS_pivoted)
  
  virgin_waste_fast <- solve(t(NGS_pivoted), generation_total$waste_generation) %>% as.data.frame() %>% 
    cbind(generation_total$country) %>%
    rename(country = "generation_total$country", virgin_waste = ".")
  virgin_waste_fast$country <- as.character(virgin_waste_fast$country)
  
  abs_probs_fast <- as.matrix(N %*% R)
  abs_probs_fast <- abs_probs_fast[substr(rownames(abs_probs_fast), 1, 2) == "G1",] %>%
    as.data.frame()
  
  abs_probs_fast <- abs_probs_fast %>%
    mutate(country = substr(row.names(abs_probs_fast), 3, 5)) %>%
    select(-Managed)
  res_detailed <- left_join(abs_probs_fast, virgin_waste_fast, by = "country") %>%
    mutate(total_mismanaged = Mismanaged * virgin_waste) %>% 
    left_join(generation_total, by = "country") %>% 
    rename(mismanagement_rate = Mismanaged)
  res_summary <- c(sum(res_detailed$total_mismanaged), sum(res_detailed$virgin_waste), sum(res_detailed$waste_generation))
  result_data <- list(detailed = res_detailed, summary = res_summary, fundamental_matrix = N)  
  
  return(result_data)


} 
# make_dtmc <- function(rates, trade_data, transitions_template){
#   transitions <- transitions_template
#   all_transitions <- data.frame(from = character(0),
#                                 to = character(0),
#                                 probability = numeric(0),
#                                 country = character(0))
#   rates <- arrange(rates, country)
#   countries <- rates$country
#   for (country in countries){
#     transitions$country <- country
#     transitions$probability <- 0
#     r <- rates[rates$country == country,]
# 
# 
#     transitions[transitions$from == "G1" & transitions$to == "Mismanaged", 3] <- (mismanagement_rate) * (1 - r$C1_rate)
#     transitions[transitions$from == "G1" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$C1_rate)
#     transitions[transitions$from == "G1" & transitions$to == "C1", 3] <- r$C1_rate
# 
#     transitions[transitions$from == "G2" & transitions$to == "Mismanaged", 3] <- (r$mismanagement_rate) * (1 - r$C2_rate)
#     transitions[transitions$from == "G2" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 -  r$C2_rate)
#     transitions[transitions$from == "G2" & transitions$to == "C2", 3] <- r$C2_rate
# 
#     transitions[transitions$from == "G3" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate)
#     transitions[transitions$from == "G3" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate
# 
#     transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "C1" & transitions$to == "G2", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
#     C1_disposed = 1 - sum(transitions[transitions$from == "C1", "probability"])
# 
#     transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- transitions[transitions$from == "C1" & transitions$to == "G1", 3] * (1 - r$trade_fraction)
#     transitions[transitions$from == "C1" & transitions$to == "G2", 3] <-  transitions[transitions$from == "C1" & transitions$to == "G2", 3] * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C1" & transitions$to == "Managed", 3] <- C1_disposed * (1 - r$mismanagement_rate) * (1 - r$trade_fraction)
#     transitions[transitions$from == "C1" & transitions$to == "Mismanaged", 3] <- C1_disposed * (r$mismanagement_rate) * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "C2" & transitions$to == "G3", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
#     C2_disposed = 1 - sum(transitions[transitions$from == "C2", "probability"])
# 
#     transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- transitions[transitions$from == "C2" & transitions$to == "G1", 3] * (1 - r$trade_fraction)
#     transitions[transitions$from == "C2" & transitions$to == "G3", 3] <-  transitions[transitions$from == "C2" & transitions$to == "G3", 3] * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C2" & transitions$to == "Managed", 3] <- C2_disposed * (1 - r$mismanagement_rate) * (1 - r$trade_fraction)
#     transitions[transitions$from == "C2" & transitions$to == "Mismanaged", 3] <- C2_disposed * (r$mismanagement_rate) * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "I" & transitions$to == "G1", 3] <- r$I_C1 * r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "I" & transitions$to == "G2", 3] <- r$I_C1 * (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
# 
#     transitions[transitions$from == "I" & transitions$to == "Mismanaged", 3] <- (r$I_mismanagement) * ((1 - r$I_C1) + (r$I_C1 * r$fraction_recycling_chemical * (1 - r$chemical_yield_rate)) + r$I_C1 * (1 - r$fraction_recycling_chemical) * (1 - r$mechanical_yield_rate)) #(amount that didn't get recycled + amount lost during chemical recycling + amount lost during mechanical recycling) * mismanagement rate
#     transitions[transitions$from == "I" & transitions$to == "Managed", 3] <- (1 - r$I_mismanagement) * ((1 - r$I_C1) + (r$I_C1 * r$fraction_recycling_chemical * (1 - r$chemical_yield_rate)) + r$I_C1 * (1 - r$fraction_recycling_chemical) * (1 - r$mechanical_yield_rate))
# 
#     transitions[transitions$from == "Mismanaged" & transitions$to == "Mismanaged", 3] <- 1
#     transitions[transitions$from == "Managed" & transitions$to == "Managed", 3] <- 1
#     all_transitions <- rbind(all_transitions, transitions)
#   }
# 
# 
#   # concatenates processing state and country code in from and to columns
#   all_transitions[!(from %in% c("Managed", "Mismanaged")),]$from <- paste(all_transitions[!(from %in% c("Managed", "Mismanaged")),]$from, all_transitions[!(from %in% c("Managed", "Mismanaged")),]$country, sep = "_")
#   all_transitions[!(to %in% c("Managed", "Mismanaged")),]$to <- paste(all_transitions[!(to %in% c("Managed", "Mismanaged")),]$to, all_transitions[!(to %in% c("Managed", "Mismanaged")),]$country, sep = "_")
#   all_transitions <- select(all_transitions, -country)
# 
#   # appends trade data
#   trade_data <- left_join(trade_data, select(rates, country, trade_fraction), by = c("from" = "country"))
#   #trade_data$trade_fraction[is.na(trade_data$trade_fraction)] <- 0
#   trade_data$probability <- trade_data$probability * trade_data$trade_fraction
#   trade_data <- select(trade_data, from, to, probability)
# 
#   trade_data1 <- trade_data
#   trade_data2 <- trade_data
#   trade_data1$from <- paste("C1_", trade_data$from, sep = "")
#   trade_data1$to <- paste("I_", trade_data$to, sep = "")
#   trade_data2$from <- paste("C2_", trade_data$from, sep = "")
#   trade_data2$to <- paste("I_", trade_data$to, sep = "")
# 
# 
#   all_transitions <- rbind(all_transitions, trade_data1, trade_data2)
#   all_transitions <- unique(all_transitions) #remove duplicate sink states
# 
#   # create the transition matrix from the data frame
#   P <- pivot_wider(all_transitions, names_from = to, values_from = probability) %>% as.data.frame()
#   P[is.na(P)] = 0
#   rownames(P) <- P[,1]
#   P <- P[,-1]
#   P <- as.matrix(P)
#    # min(apply(P, 1, sum))
#    # max(apply(P, 1, sum))
#    # asdf <- apply(P, 1, sum) %>% as.data.frame()
#    # asdf <- asdf %>%
#    #   mutate(state = row.names(asdf)) %>%
#    #   rename(total_prob = ".") %>%
#    #   filter(total_prob != 1)
# 
# 
#   return(P[order(row.names(P)), order(colnames(P))])
# }



# make_dtmc <- function(rates, trade_data, transitions_template){
#   transitions <- transitions_template
#   all_transitions <- data.frame(from = character(0),
#                                 to = character(0),
#                                 probability = numeric(0),
#                                 country = character(0))
#   rates <- arrange(rates, country)
#   countries <- rates$country
#   for (country in countries){
#     transitions$country <- country
#     transitions$probability <- 0
#     r <- rates[rates$country == country,]
# 
# 
#     transitions[transitions$from == "G1" & transitions$to == "Mismanaged", 3] <- (r$mismanagement_rate) * (1 - r$C1_rate)
#     transitions[transitions$from == "G1" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$C1_rate)
#     transitions[transitions$from == "G1" & transitions$to == "C1", 3] <- r$C1_rate
# 
#     transitions[transitions$from == "G2" & transitions$to == "Mismanaged", 3] <- (r$mismanagement_rate) * (1 - r$C2_rate)
#     transitions[transitions$from == "G2" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 -  r$C2_rate)
#     transitions[transitions$from == "G2" & transitions$to == "C2", 3] <- r$C2_rate
# 
#     transitions[transitions$from == "G3" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate)
#     transitions[transitions$from == "G3" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate
# 
#     transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "C1" & transitions$to == "G2", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
#     C1_disposed = 1 - sum(transitions[transitions$from == "C1", "probability"])
# 
#     transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- transitions[transitions$from == "C1" & transitions$to == "G1", 3] * (1 - r$trade_fraction)
#     transitions[transitions$from == "C1" & transitions$to == "G2", 3] <-  transitions[transitions$from == "C1" & transitions$to == "G2", 3] * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C1" & transitions$to == "Managed", 3] <- C1_disposed * (1 - r$mismanagement_rate) * (1 - r$trade_fraction)
#     transitions[transitions$from == "C1" & transitions$to == "Mismanaged", 3] <- C1_disposed * (r$mismanagement_rate) * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "C2" & transitions$to == "G3", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
#     C2_disposed = 1 - sum(transitions[transitions$from == "C2", "probability"])
# 
#     transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- transitions[transitions$from == "C2" & transitions$to == "G1", 3] * (1 - r$trade_fraction)
#     transitions[transitions$from == "C2" & transitions$to == "G3", 3] <-  transitions[transitions$from == "C2" & transitions$to == "G3", 3] * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "C2" & transitions$to == "Managed", 3] <- C2_disposed * (1 - r$mismanagement_rate) * (1 - r$trade_fraction)
#     transitions[transitions$from == "C2" & transitions$to == "Mismanaged", 3] <- C2_disposed * (r$mismanagement_rate) * (1 - r$trade_fraction)
# 
#     transitions[transitions$from == "I" & transitions$to == "G1", 3] <- r$I_C1 * r$fraction_recycling_chemical * r$chemical_yield_rate
#     transitions[transitions$from == "I" & transitions$to == "G2", 3] <- r$I_C1 * (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
# 
#     transitions[transitions$from == "I" & transitions$to == "Mismanaged", 3] <- (r$I_mismanagement) * ((1 - r$I_C1) + (r$I_C1 * r$fraction_recycling_chemical * (1 - r$chemical_yield_rate)) + r$I_C1 * (1 - r$fraction_recycling_chemical) * (1 - r$mechanical_yield_rate)) #(amount that didn't get recycled + amount lost during chemical recycling + amount lost during mechanical recycling) * mismanagement rate
#     transitions[transitions$from == "I" & transitions$to == "Managed", 3] <- (1 - r$I_mismanagement) * ((1 - r$I_C1) + (r$I_C1 * r$fraction_recycling_chemical * (1 - r$chemical_yield_rate)) + r$I_C1 * (1 - r$fraction_recycling_chemical) * (1 - r$mechanical_yield_rate))
# 
#     transitions[transitions$from == "Mismanaged" & transitions$to == "Mismanaged", 3] <- 1
#     transitions[transitions$from == "Managed" & transitions$to == "Managed", 3] <- 1
#     all_transitions <- rbind(all_transitions, transitions)
#   }
# 
# 
#   # concatenates processing state and country code in from and to columns
#   all_transitions[!(from %in% c("Managed", "Mismanaged")),]$from <- paste(all_transitions[!(from %in% c("Managed", "Mismanaged")),]$from, all_transitions[!(from %in% c("Managed", "Mismanaged")),]$country, sep = "_")
#   all_transitions[!(to %in% c("Managed", "Mismanaged")),]$to <- paste(all_transitions[!(to %in% c("Managed", "Mismanaged")),]$to, all_transitions[!(to %in% c("Managed", "Mismanaged")),]$country, sep = "_")
#   all_transitions <- select(all_transitions, -country)
# 
#   # appends trade data
#   trade_data <- left_join(trade_data, select(rates, country, trade_fraction), by = c("from" = "country"))
#   #trade_data$trade_fraction[is.na(trade_data$trade_fraction)] <- 0
#   trade_data$probability <- trade_data$probability * trade_data$trade_fraction
#   trade_data <- select(trade_data, from, to, probability)
# 
#   trade_data1 <- trade_data
#   trade_data2 <- trade_data
#   trade_data1$from <- paste("C1_", trade_data$from, sep = "")
#   trade_data1$to <- paste("I_", trade_data$to, sep = "")
#   trade_data2$from <- paste("C2_", trade_data$from, sep = "")
#   trade_data2$to <- paste("I_", trade_data$to, sep = "")
# 
# 
#   all_transitions <- rbind(all_transitions, trade_data1, trade_data2)
#   all_transitions <- unique(all_transitions) #remove duplicate sink states
# 
#   # create the transition matrix from the data frame
#   P <- pivot_wider(all_transitions, names_from = to, values_from = probability) %>% as.data.frame()
#   P[is.na(P)] = 0
#   rownames(P) <- P[,1]
#   P <- P[,-1]
#   P <- as.matrix(P)
#    # min(apply(P, 1, sum))
#    # max(apply(P, 1, sum))
#    # asdf <- apply(P, 1, sum) %>% as.data.frame()
#    # asdf <- asdf %>%
#    #   mutate(state = row.names(asdf)) %>%
#    #   rename(total_prob = ".") %>%
#    #   filter(total_prob != 1)
# 
# 
#   return(P[order(row.names(P)), order(colnames(P))])
# }

hitProbs <- function(transitionMat, targets, absorbing){
  
  hit_probs <- data.frame(source = rownames(transitionMat)[!(rownames(transitionMat) %in% absorbing)])
  for(i in 1:length(targets)){
    
    rhs <- c(rep(0, times = ncol(transitionMat) - length(absorbing) - 1), 1)
    Q <- transitionMat[!(rownames(transitionMat) %in% c(targets[i], absorbing)), !(colnames(transitionMat) %in% c(targets[i], absorbing))]
    C <- transitionMat[!(rownames(transitionMat) %in% c(targets[i], absorbing)), colnames(transitionMat) == targets[i]]
    I <- diag(sqrt(length(Q)))
    W <- rbind(cbind((Q-I), C), rhs)
    sol <- round(lsolve.bicg(A = W, B = rhs, maxiter = 1000, verbose = FALSE)$x, digits = 4) %>% as.data.frame()
    names(sol) = targets[i]
    source <- c(rownames(Q), targets[i])
    sol <- cbind(sol, source)
    hit_probs <- left_join(hit_probs, sol, by = "source")
  }
  names(hit_probs)[2:ncol(hit_probs)] = targets
  rownames(hit_probs) <- hit_probs[,1]
  hit_probs <- hit_probs[,-1]
  hit_probs <- hit_probs[order(rownames(hit_probs)), order(colnames(hit_probs))]
  return(as.matrix(hit_probs))
}


absorptionProbs <- function(transitionMat, absorbing){
  
  Q <- transitionMat[!(rownames(transitionMat) %in% absorbing), !(colnames(transitionMat) %in% absorbing)]
  rhs <- c(rep(0, times = ncol(Q)), 1)
  C <- transitionMat[!(rownames(transitionMat) %in% absorbing), colnames(transitionMat) == absorbing[1]]
  I <- diag(nrow(Q))
  W <- rbind(cbind((Q-I), C), rhs)
  sol <- round(lsolve.bicg(A = W, B = rhs, maxiter = 1000, verbose = FALSE)$x, digits = 4) %>% as.data.frame()
  names(sol) = absorbing[1]
  source <- c(rownames(Q), absorbing[1])
  sol <- cbind(sol, source)
  sol <- sol[substr(sol$source, 1, 2) == "G1",]
  sol$Mismanaged <- 1 - sol$Managed
  
  return(sol)
}


virginWaste <- function(transitionMat, hitprobs_all, generation_total){
  total_waste <- generation_total$waste_generation
  countries <- generation_total$country
  K <- matrix(rep(0, times = length(countries)^2), nrow = length(countries))
  hitprobs_src_G1 <- hitprobs_all[substr(rownames(hitprobs_all), 1, 2) == "G1", substr(colnames(hitprobs_all), 1, 2) %in% c("G1", "G2", "G3", "I_")]
  
  #calculates return probabilities
  hg <- hitprobs_all[substr(rownames(hitprobs_all), 1, 2) %in% c("I_", "C1"), substr(colnames(hitprobs_all), 1, 2) == "G1"]
  pg <- transitionMat[substr(rownames(transitionMat), 1, 2) == "G1", substr(colnames(transitionMat), 1, 2) %in% c("I_", "C1")]
  pg <- pg[, order(colnames(pg))]
  
  return_probs <- colSums(t(pg) * hg) #equivalent result to diag(pg %*% hg)
  output_rate_mult <- 1/(1-return_probs) #calculates G1 output/virgin input ratio for each country
  
  #creates the coefficient matrix
  for(i in 1:length(countries)){
    for(j in 1:length(countries)){
      if(i == j){
        K[i, j] = output_rate_mult[i] * (1 + hitprobs_src_G1[paste0("G1_", countries[i], sep = ""), paste0("G2_", countries[i], sep = "")] +
                                           hitprobs_src_G1[paste0("G1_", countries[i], sep = ""), paste0("G3_", countries[i], sep = "")] + 
                                           return_probs[i])
      }else{
        K[i, j] = (output_rate_mult[i] * 
                     hitprobs_src_G1[paste0("G1_", countries[j], sep = ""), paste0("G1_", countries[i], sep = "")] * 
                     (1 + hitprobs_src_G1[paste0("G1_", countries[i], sep = ""), paste0("G2_", countries[i], sep = "")] +
                        hitprobs_src_G1[paste0("G1_", countries[i], sep = ""), paste0("G3_", countries[i], sep = "")])) + 
          (hitprobs_src_G1[paste0("G1_", countries[j], sep = ""), paste0("I_", countries[i], sep = "")] * 
             P[paste0("I_", countries[i], sep = ""), paste0("C2_", countries[i], sep = "")] * 
             P[paste0("C2_", countries[i], sep = ""), paste0("G3_", countries[i], sep = "")])
        
        
      }
    }
  }
  
  virgin_waste <- lsolve.bicg(A = t(K), B = total_waste, verbose = FALSE)$x
  colnames(virgin_waste) = "Qty"
  rownames(virgin_waste) <- countries
  return(virgin_waste)
}

