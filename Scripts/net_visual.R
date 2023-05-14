library(tidyverse)
library(data.table)
library(Matrix.utils)
library(ggraph)
library(igraph)
library(ragg)
library(tidygraph)
library(Cairo)
library(HiveR)


################## functions ###################
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
  
  sorting_y <- .71
  recycling_y <- .775
  Imp_misman <- .5
  I_C1_rate <- .8
  C1_rate_unknown <- .1
  
  
  
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
  rates_iter_base$C1_new <- 0
  
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
  
  all_transitions[from == "I1" & to == "G1", probability := I_C1 * fraction_recycling_chemical * chemical_yield_rate]
  all_transitions[from == "I1" & to == "G2", probability := I_C1 * (1 - fraction_recycling_chemical) * mechanical_yield_rate]
  
  all_transitions[from == "I1" & to == "Mismanaged", probability := (I_mismanagement) * ((1 - I_C1) + (I_C1 * fraction_recycling_chemical * (1 - chemical_yield_rate)) + I_C1 * (1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate))] #(amount that didn't get recycled + amount lost during chemical recycling + amount lost during mechanical recycling) * mismanagement rate
  all_transitions[from == "I1" & to == "Managed", probability := (1 - I_mismanagement) * ((1 - I_C1) + (I_C1 * fraction_recycling_chemical * (1 - chemical_yield_rate)) + I_C1 * (1 - fraction_recycling_chemical) * (1 - mechanical_yield_rate))] 
  
  
  
  
  
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
                    to = paste0(rep("I1", times = length(trade_data[,from])), to))]
  
  probs <- rbindlist(list(probs, trade_data[, c("from", "to", "probability")])) #attach trade transition probabilities to full list
  
  # create the transition matrix from the data frame
  P <- dcast(probs, from ~ to, value.var = "probability", fill = 0)
  P <- as.matrix(P[, -1], rownames.value = P[, from])
  
  # min(apply(P, 1, sum))
  # max(apply(P, 1, sum))
  # asdf <- apply(P, 1, sum) %>% as.data.frame()
  # asdf <- asdf %>%
  #   mutate(state = row.names(asdf)) %>%
  #   rename(total_prob = ".") %>%
  #   filter(total_prob != 1)
  
  
  return(list(mat = P, adj = probs))
}

make_model <- function(P, generation_total){  
  
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
  
  
  virgin_waste_fast <- solve(t(G_throughput), generation_total)
  abs_probs_fast <- N %*% R
  
  return(list(fundamental = N, vwg = virgin_waste_fast, abs = abs_probs_fast))
  
} 



mod.edge2HPD <- function(edge_df = NULL, 
                         node.attributes, 
                         # unique.rows = TRUE, 
                         axis.cols = NULL, 
                         type = "2D", 
                         desc = NULL) 
{
  #edge.weight - a list corresponding to edge weights (same order as in edge_df)
  #edge.color - a lis corresponding to edge colors (same order as in edge_df)
  #node.color - a data frame consisting of two columns: column 1 - node labels, column 2 - node color
  #node.size - a data frame consisting of two columns: column 1 - node labels, column 2 - node size
  #node.radius - a data frame consisting of two columns: column 1 - node labels, column 2 - node radius
  #node.axis - a data frame consisting of two columns: column 1 - node labels, column 2 - node axis
  
  
  # if (unique.rows)
  # {
  #   nr.old <- nrow(edge_df)
  #   edge_df <- unique(edge_df)
  # 
  #   if (nr.old > nrow(edge_df))
  #     cat("\n\t", nr.old - nrow(edge_df), "non-unique data-frame rows removed!\n\n")
  # }
  
  
  HPD <- list()
  
  # Define node attributes
  HPD$nodes$id <- as.integer(node.attributes$id)
  HPD$nodes$lab <- as.character(node.attributes$label)
  HPD$nodes$axis <- as.integer(node.attributes$axis)
  HPD$nodes$radius <- as.numeric(node.attributes$radius)
  HPD$nodes$size <- as.numeric(node.attributes$size)
  HPD$nodes$color <- as.character(node.attributes$color)
  
  
  ####################################################
  # # Set up edge list
  # # Merge by default sorts things and changes the order of edges, so edge list has to stay paired
  # edge.hlp <- merge(edge_df, node.attributes[, 1:2], by.x = 1, by.y = "label")
  # edge <- merge(edge.hlp, node.attributes[1:2], by.x = 2, by.y = "label")
  # 
  HPD$edges$id1 <- as.integer(edge_df$id1)
  HPD$edges$id2 <- as.integer(edge_df$id2)
  
  HPD$edges$weight <- as.numeric(edge_df$weight)
  HPD$edges$color <- as.character(edge_df$color)
  
  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)
  
  # Add description
  if (is.null(desc)) {
    desc <- "No description provided"
  }
  HPD$desc <- desc
  
  # Define axis columns
  if (is.null(axis.cols)){
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
  }
  
  
  HPD$axis.cols <- axis.cols
  HPD$nodes$axis <- as.integer(HPD$nodes$axis)
  HPD$nodes$size <- as.numeric(HPD$nodes$size)
  HPD$nodes$color <- as.character(HPD$nodes$color)
  HPD$nodes$lab <- as.character(HPD$nodes$lab)
  HPD$nodes$radius <- as.numeric(HPD$nodes$radius)
  HPD$nodes$id <- as.integer(HPD$nodes$id)
  HPD$edges$id1 <- as.integer(HPD$edges$id1)
  HPD$edges$id2 <- as.integer(HPD$edges$id2)
  HPD$edges$weight <- as.numeric(HPD$edges$weight)
  HPD$edges$color <- as.character(HPD$edges$color)
  HPD$type <- type
  
  class(HPD) <- "HivePlotData"
  
  # Check HPD object
  chkHPD(HPD)
  return (HPD)
}


################## data ########################
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
         I_mismanagement_max) %>%
  arrange(-waste_generation) %>%
  head(50)



rate_data$C1_rate[rate_data$country == "CHN"] = 0.3 # https://www.reuters.com/article/uk-china-environment-plastics-idUKKBN28A13X
rate_data[rate_data$country == "IND", "C1_rate"] = 0.1
eu_mismanagement <- read_csv("Data/eu_mismanagement_rates.csv")
countries <- rate_data$country

twg <- as.matrix(rate_data[order(rate_data$country),"waste_generation"])
rownames(twg) <- rate_data[,"country"]
twg


#pre-populates a list of transitions to make assigning their respective probabilities much faster
processes <- c("G1","G2", "G3", "C1", "C2", "I1", "Managed", "Mismanaged")
transitions_list <- data.frame(from = rep(processes[1:6], times = 8),
                               to = rep(processes, each = 6),
                               probability = numeric(48), 
                               country = rep(countries, each = 48))

trade_year = 2019 # which year's trade data to default to
trade_flow_data <- read_csv(paste("Data/trade_clean/trade_probs", trade_year, ".csv", sep = "")) %>%
  filter(from %in% countries, to %in% countries) %>%
  left_join(select(rate_data, country, mismanagement_rate), by = c("from" = "country")) %>%
  rename(exporter_mismanagement = mismanagement_rate) %>%
  left_join(select(rate_data, country, mismanagement_rate), by = c("to" = "country")) %>%
  rename(importer_mismanagement = mismanagement_rate) %>%
  filter(from != "NAM") %>% #data contains zero flows for NAM
  as.data.frame()

trade_flow_data[trade_flow_data$from == "EU", "exporter_mismanagement"] = eu_mismanagement[eu_mismanagement$year == 2019, "r"]
trade_flow_data[trade_flow_data$to == "EU", "importer_mismanagement"] = .05741
trade_flow_main <- filter(trade_flow_data, quantity > .001*sum(trade_flow_data$quantity)) %>%
  select(-exports)
exp <- trade_flow_main %>%
  group_by(from) %>%
  summarise(exports = sum(quantity))
trade_flow_main <- left_join(trade_flow_main, exp)
trade_flow_main <- mutate(trade_flow_main, probability = quantity / exports)
filled <- baseline_fill(year = trade_year, trade_probs = trade_flow_main, rates_unfilled = rate_data)$mat
# filled <- initialized$mat
# params <- initialized$params
#filled$trade_fraction <- 0
filled$fraction_recycling_chemical <- 0*(0.5 - filled$C1_rate) / 0.5 *.75
#filled$C1_rate <- 0.5
#filled$mechanical_yield_rate <- 0.9 * 0.9
#filled$chemical_yield_rate <- 0.9 * 0.9
# params <- c(0.9, 0.9, params[3:4], 0.5)
# vals = make_model(rates_iter = rates_filled, generation_total = twg, trade = trade_flow_data)
# results[results$iter == i & results$scenario == "increase mech, chem, yield", 2:4] = vals
# results[results$iter == i & results$scenario == "increase mech, chem, yield", 5:9] = params


flows <- merge.Matrix(transitions_list, filled, by.x = transitions_list[, "country"], by.y = filled[, "country"])[, -c(5, 7)]

#flows$trade_fraction <- 0




probs <- make_dtmc(transitions = flows, trade_data = as.data.table(trade_flow_main))

res <- make_model(P = probs$mat, generation_total = twg)

fundamental <- res$fundamental
fundamental <- fundamental[order(rownames(fundamental)), order(colnames(fundamental))]
vwg <- res$vwg
vwg_full <- matrix(c(rep(0, times = 2 * length(countries)), vwg, rep(0, times = 3 * length(countries))), ncol = 1)
rownames(vwg_full) <- rownames(fundamental)
colnames(vwg_full) <- "initial"
abs <- res$abs[((length(countries) * 2) + 1):(length(countries)*3),]
total_mis = sum(vwg * abs)
total_proper = sum(vwg * (1 - abs))
mc <- probs$adj
tp <- data.frame(tput = c(t(fundamental) %*% vwg_full, sum(vwg)),
                         node = c(rownames(t(fundamental)), "Source")) 

#################### make the bad graph ##########################




adjList <- probs$adj %>%
  rbind(data.frame(to = paste0("G1", rownames(vwg)),
                   from = "Source",
                   probability = vwg / sum(vwg))) %>%
  mutate(flow_type = ifelse(substr(to, 1, 2) == "G1", "Source",
                            ifelse(to == "Mismanaged", "Mismanaged",
                                   ifelse(to == "Managed", "Properly Managed",
                                          ifelse(substr(to, 1, 1) == "C", "Recycling",
                                                 ifelse(substr(to, 1, 1) == "I", "Imports", "Regeneration")))))) %>%
  left_join(throughput, by = c("from" = "node")) %>%
  mutate(weight = probability * tput) %>%
  select(-probability, -tput) %>%
  filter(weight > 0) %>%
  rbind(data.frame(from = c("Mismanaged", "Managed"),
                   to = c("Mismanaged", "Managed"),
                   flow_type = c("Mismanagement", "Proper Management"),
                   weight = c(1, 1)))
 
         
nodeAttrs <- data.frame(node = unique(adjList$from)) %>%
               mutate(ntype = ifelse(substr(node, 1, 2) == "G1", "Source",
                                     ifelse(node == "Mismanaged", "Mismanaged",
                                            ifelse(node == "Managed", "Properly Managed",
                                                   ifelse(substr(node, 1, 1) == "C", "Recycling",
                                                          ifelse(substr(node, 1, 1) == "I", "Imports", "Regeneration"))))),
                      country = ifelse(node %in% c("Mismanaged", "Managed"), "Sink", substr(node, nchar(node) - 2, nchar(node)))) %>%
  left_join(throughput) 
                    
nodeAttrs$country[substr(nodeAttrs$country, 2, 3) == "EU"] <- "EU"                      
#nodeAttrs$ntype = factor(nodeAttrs$ntype, levels = c("Mismanaged", "Imports", "Properly Managed", "Regeneration", "Recycling", "Source"))
nodeAttrs[nodeAttrs$node == "Mismanaged", "tput"] = total_mis
nodeAttrs[nodeAttrs$node == "Managed", "tput"] = total_proper



# 
# adjList <- adjList %>% filter(!(from %in% c("Managed", "Mismanaged")) & !(to %in% c("Managed", "Mismanaged")))
# nodeAttrs <- nodeAttrs %>% filter(!(node %in% c("Managed", "Mismanaged")))
# 


# P <- probs$mat
# 
# c_sample <- sample(countries, 5)
# smallP <- P[(substr(rownames(P), 3, 5) %in% c_sample) | rownames(P) %in% c("Mismanaged", "Managed"),(substr(colnames(P), 3, 5) %in% c_sample) | colnames(P) %in% c("Mismanaged", "Managed")]
# min(apply(smallP, 1, sum))
# max(apply(smallP, 1, sum))
# asdf <- apply(smallP, 1, sum) %>% as.data.frame()
# asdf <- asdf %>%
#   mutate(state = row.names(asdf)) %>%
#   rename(total_prob = ".") %>%
#   filter(total_prob != 1)
# 
# 
# gr <- graph_from_adjacency_matrix(smallP, mode = "directed", weighted = TRUE)
# V(gr)$ntype <- ifelse(substr(V(gr)$name, 1, 2) == "G1", "Source",
#                       ifelse(V(gr)$name == "Mismanaged", "Mismanaged",
#                              ifelse(V(gr)$name == "Managed", "Properly Managed",
#                                     ifelse(substr(V(gr)$name, 1, 1) == "C", "Recycling",
#                                            ifelse(substr(V(gr)$name, 1, 1) == "I", "Imports", "Regeneration")))))
# E(gr)$etype <- ifelse(substr(ends(gr, E(gr))[,2], 1, 1) == "C", "Recycling",
#                       ifelse(ends(gr, E(gr))[,2] == "Mismanaged", "Mismanagement",
#                              ifelse(ends(gr, E(gr))[,2] == "Managed", "Properly Management",
#                                     ifelse(substr(ends(gr, E(gr))[,2], 1, 1) == "I", "Trade", "Regeneration"))))
#                                            

gr <- graph_from_data_frame(adjList, directed = TRUE, vertices = nodeAttrs) %>% as_tbl_graph() 
set_graph_style(plot_margin = margin(1,1,1,10))
l <- create_layout(gr, layout = "tree")


ggraph(l) + 
  geom_edge_diagonal(aes(alpha = weight,
                    color = as.character(flow_type),
                    width = weight), 
                    show.legend = F) +
  geom_node_point(aes(color = ntype, size = tput), show.legend = c("color" = T, "size" = F)) +
  scale_edge_color_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  scale_edge_width(range = c(.85, 1.15)) +
  scale_alpha(range = c(.95, 1)) +
  theme_void() +
  labs(color = "State")



gr






########################## three country ##################
clist <- c("_A", "_B", "_C")

#pre-populates a list of transitions to make assigning their respective probabilities much faster
processes <- c("G1","G2", "G3", "C1", "C2", "I", "Managed", "Mismanaged")
connections <- data.frame(from = rep(processes, times = 8),
                               to = rep(processes, each = 8),
                               weight = numeric(64), 
                               country = rep(clist, each = 64)) %>% 
  rbind(data.frame(from = rep("Source", times = 3),
                   to = rep("G1", times = 3),
                   weight = c(1, 1, 1), 
                   country = clist)) %>% as.data.table()




connections[(from == "G1" & to %in% c("C1", "Managed", "Mismanaged")) |
            (from == "G2" & to %in% c("C2", "Managed", "Mismanaged")) | 
            (from == "G3" & to %in% c("Managed", "Mismanaged")) | 
            (from == "C1" & to %in% c("Managed", "Mismanaged", "G1", "G2")) | 
            (from == "C2" & to %in% c("Managed", "Mismanaged", "G1", "G3")) |
            (from == "I" & to %in% c("Managed", "Mismanaged", "G1", "G2")) |
            (from == "Managed" & to == "Managed") |
            (from == "Mismanaged" & to == "Mismanaged"), weight := 1]

connections <- connections[weight > 0,]
connections[!(from %in% c("Mismanaged", "Managed", "Source")), from := paste0(from, country)
            ][!(to %in% c("Mismanaged", "Managed", "Source")), to := paste0(to, country)]
connections <- unique(connections[, -"country"]) 

trade_flows <- data.frame(from = rep(c("C1", "C2"), times = 6),
                          to = rep("I", times = 12),
                          fcountry = rep(clist, each = 4),
                          tcountry = c("_B", "_B", "_C", "_C", "_A", "_A", "_C", "_C", "_A", "_A", "_B", "_B"),
                          weight = 1)
trade_flows$from <- paste0(trade_flows$from, trade_flows$fcountry)  
trade_flows$to <- paste0(trade_flows$to, trade_flows$tcountry)  
trade_flows <- select(trade_flows, -fcountry, -tcountry)                                  
#connections <- rbind(connections, trade_flows)                                  
        
attrs <- data.frame(node = unique(connections$from))  %>%  
   mutate(State = ifelse(substr(node, 1, 2) == "G1", "VWG (G1)",
                        ifelse(node == "Mismanaged", "Mismanaged (MM)",
                               ifelse(node == "Managed", "Managed Properly (MP)",
                                      ifelse(substr(node, 1, 1) == "C", "Recycling (R1/R2)",
                                             ifelse(substr(node, 1, 1) == "I", "Imports (I)", 
                                                    ifelse(node == "Source", "Source","Regeneration (G2/G3)")))))),
         country = ifelse(node %in% c("Mismanaged", "Managed"), "Sinks", 
                          ifelse(node == "Source", "Source", paste0("Country ",substr(node, nchar(node), nchar(node)))))) %>%
  filter(node != "Source")

connections <- left_join(connections, select(attrs, State, node), by = c("to" = "node")) %>% filter(from != "Source")                  
small <- graph_from_data_frame(connections, vertices = attrs) %>% as_tbl_graph()                               

set_graph_style(plot_margin = margin(100, 100, 100, 100))
pal <- RColorBrewer::brewer.pal(6, 'RdYlBu')
pal2 <- c(pal[1:3], pal[5:6])
pal3 <- RColorBrewer::brewer.pal(4, 'YlGnBu')
l <- create_layout(small, layout = "fr")
l$State <- factor(l$State, levels = c("Mismanaged (MM)", "Managed Properly (MP)", "Recycling (R1/R2)", "Imports (I)", "Regeneration (G2/G3)", "VWG (G1)"))

netVis <- ggraph(l) + 
  theme_graph(title_family = "Calibri", title_size = 30) + 
  # labs(title = "Markov chain structure, three countries") +
  theme(plot.margin=unit(c(.5, .5, .5, .5),"cm"), text = element_text(size = 25)) + 
  geom_edge_diagonal(aes(color = as.factor(State)), show.legend = F, alpha = .7) +
  geom_node_point(aes(color = State), size = 7) +
#  geom_mark_rect(aes(x, y, group = country), fill = "Blue", size = .05, alpha = .05, label.fill = "White", label.fontsize = 15, label.buffer = unit(.2, "cm"), con.type = "straight", con.border = "all", label.lineheight = 20) +
  scale_color_manual(values = pal) +
  scale_edge_color_manual(values = pal2) +
  scale_fill_manual(values = pal3) 
print(netVis)
  
Cairo(width = 2800, height = 2000, file="Figures/mc_three_Countries.png", type = "png", pointsize=12, 
      bg = "transparent", canvas = "white", units = "px", dpi = 300)
print(netVis)
dev.off()
                               
                                  
                                  
                                  
                                  
                                  
                                  
                                  










################## hive plot #############


library(RColorBrewer)
library(grid)
library(viridis)

# pal <- RColorBrewer::brewer.pal(6, 'RdYlBu')
# pal2 <- c(pal[1], pal[4:2], pal[4], pal[2], pal[5:6])
# 


#including imports
edges <- probs$adj %>% rename(weight = probability) %>% as.data.table()



edges <- edges[from != to & weight > 0, ]
#edges <- edges[!(substr(from, 1, 2) %in% c("G3", "C2")) & !(substr(to, 1, 2) %in% c("G3", "C2")), ]

# dup <- copy(edges[substr(from, 1, 1) == "I", ])
# dup[, from := paste("I2", substr(from, 2, nchar(from)), sep = "")]
# dup[substr(to, 1, 2) == "G2", to := paste("G3", substr(to, 3, nchar(to)), sep = "")]

# edges[substr(from, 1, 2) == "C1" & substr(to, 1, 1) == "I", to := paste("I1", substr(to, 2, nchar(to)), sep = "")]
# edges[substr(from, 1, 2) == "C2" & substr(to, 1, 1) == "I", to := paste("I2", substr(to, 2, nchar(to)), sep = "")]
# edges <- edges[substr(from, 1, 1) == "I", from :=  paste("I1", substr(from, 2, nchar(from)), sep = "")]
# edges <- rbind(edges, dup)

throughput <- copy(as.data.table(tp))
rw=data.frame(tput = throughput[node=="Source", tput], node = "Sink")
throughput <- rbind(throughput,rw)

# dupT <- copy(throughput[substr(node, 1, 1) == "I", ])[substr(node, 1, 1) == "I", ':='(node = paste("I2", substr(node, 2, nchar(node)), sep = ""),
#                                                                                       tput = 0.1 * tput)]
# throughput <- throughput[substr(node, 1, 1) == "I",':='(node = paste("I1", substr(node, 2, nchar(node)), sep = ""),
#                                                         tput = 0.9 * tput)]
# throughput <- rbind(throughput, dupT)
nodeAttrs <- data.table(label = unique(c(edges$from, edges$to))) %>%
  mutate(axis = ifelse(substr(label, 1, 1) == "G", 1,
                       ifelse(substr(label, 1, 1) %in% c("C", "I"), 2, 3))) %>%
  
  left_join(rename(throughput, label = node))


nodeAttrs[nodeAttrs$label == "Mismanaged", "tput"] = total_mis
nodeAttrs[nodeAttrs$label == "Managed", "tput"] = total_proper
nodeAttrs <- nodeAttrs[tput > 0,][, id := 1:(.N)]


edges <- edges[(from %in% nodeAttrs$label) & (to %in% nodeAttrs$label),]
# pal <- viridis(n = 7, alpha = 1, begin = .75, end = 1, option = "D")
pal <- brewer.pal(8, "Spectral")

nodeAttrs[, ':=' (radius = ifelse(substr(label, 2, 2) == "1", runif(nrow(nodeAttrs[substr(label, 2, 2) == "1", ]), 0, .5), 
                                  ifelse(label == "Mismanagement", 0,
                                         ifelse(label == "Management", .8,
                                                ifelse(substr(label, 1, 1) == "I", runif(substr(label, 1, 1) == "I", .7, 1),
                                                       ifelse(substr(label, 1, 2) == "C2" | substr(label, 1, 2) == "G3" , runif(substr(label, 1, 2) == "C2" | substr(label, 1, 2) == "G3", 1.1, 1.6),
                                                              runif(substr(label, 1, 2) == "G2", .7, 1)))))),
                  size = .25 + 5.5 * (tput - min(tput))/(max(tput) - min(tput)),
                  color = ifelse(substr(label, 1, 1) == "G", pal[3],
                                 # ifelse(substr(label, 1, 2) == "G2", pal[1],
                                        # ifelse(substr(label, 1, 2) == "G3", pal[3],
                                               ifelse(substr(label, 1, 1) == "C", pal[7],
                                                      ifelse(substr(label, 1, 1) == "I", pal[8], 
                                                             ifelse(label == "Managed", pal[2], pal[1])))))]


nodeAttrs[substr(label, 1, 1) == "I", radius := runif(substr(label, 1, 1) == "I", .6, 1)]



edges.hlp <- nodeAttrs[, .(label, id)][copy(edges), on = c("label" = "from")] %>% setnames(old = c("label", "id"), new = c("from", "id1"))
edges.hlp <- left_join(edges.hlp, nodeAttrs[, .(label, id)], by = c("to" = "label")) %>% setnames(old = "id", new = "id2") %>% copy()
edges.hlp[, color := ifelse(substr(to, 1, 1) == "G", pal[6],
                            ifelse(substr(to, 1, 1) == "C", pal[8],
                                 ifelse(substr(to, 1, 1) == "C", pal[8],
                                        ifelse(substr(from, 1, 1) == "C" & substr(to, 1, 1) == "I", pal[3],
                                        # ifelse(substr(label, 1, 2) == "G3", pal[3],
                                              ifelse(to == "Mismanaged", pal[1], pal[2])))))
          ][, weight := 1 * weight]

edges.hlp <- left_join(edges.hlp, throughput, by  = c("from" = "node"))

edges.hlp <- edges.hlp[weight > 0,][, weight := weight * tput][, weight := 3.4 * (weight / max(weight))^(.5)]
                                                               # ][, weight := -log(1 *weight)
                                                               #   ][, weight := weight / max(weight)]
# edge.hlp$color <- "#A1FF71" 
nodeAttrs[, size := 1.5 * size^(1)]

hive1 <- mod.edge2HPD(edge_df = edges.hlp,
                      node.attributes = nodeAttrs,
                      # desc = "useless",
                      axis.cols = "black",
                      type = "2D")
hive1 <- manipAxis(hive1, method = "norm")
# hive1 <- manipAxis(hive1, method = "scale", action = c(1.5, 1.5, 1))
hive1 <- manipAxis(hive1, method = "stretch", action = c(1.3, .8, 1))

plotHive(hive1,
         dr.nodes = T,
         ch = 0.45,
         
         bkgnd = "white")
# 
# 
# 
# 
# hive2 <- mineHPD(hive1, option =  "axis <- source.man.sink")
# plotHive(hive2, ch =0,
#          # dr.nodes = T,
#            # np = T,
#          # method = "abs", 
#          bkgnd = "white"
#          )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# diag(fundamental) <- 0
# fd <- probs$mat
# fd <- data.table(from = rownames(fd), fd) %>% melt(id.vars = "from",
#                                                                      variable.name = "to",
#                                                                      value.name = "p")
# fd <- fd[p > 0,]
# fd[, p := (p - min(p))/(max(p) - min(p)) + 2.5]
# # fd$p <- fd$p * 1000
# # fd <- fd[from != to & from != "I" & to != "I",]
# fd <- as.data.frame(fd)
# fd[!( fd$from %in% nodeAttrs$node ), ]
# 
# nodeAttrs$ntype <- as.integer(nodeAttrs$ntype)
# nodeAttrs <- nodeAttrs[(nodeAttrs$node %in% fd$from) | (nodeAttrs$node %in% fd$to), ]
# hive1 <- mod.edge2HPD(edge_df = fd[, 1:2], edge.weight = fd[, 3], node.size = nodeAttrs[, c("node", "tput")], node.axis = nodeAttrs[, c("node", "ntype")] )
# 
# library(RColorBrewer)
# 
# 
# hive2 <- mineHPD(hive1, option = "remove zero edge")
# 
# plotHive(hive2, method = "abs", bkgnd = "white",  axLab.pos = 1)
# 
# 
# 
# 
# 
# hive1 <- edge2HPD(edge_df = fd)
# hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
# hive2$nodes$axis <- sapply(hive2$nodes$lab, 
#                            function(x){
#                              if(substr(x, 1, 1) == "I"){
#                                s = "I"
#                              }else{
#                                s <- substr(x, 1, 2)
#                              }
#                              y <- ifelse(s == "G1", 1,
#                                          ifelse(s == "C1", 2,
#                                                 ifelse(s == "G2", 3, 
#                                                        ifelse(s == "C2", 4, 
#                                                               ifelse(s == "G3", 5, 5)))))
#                               y
#                             })
# 
# plotHive(hive2, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), axLab.pos = 1)
# 
# 
# hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
# 
# 




################# Circos plots ##########

#including imports



edges <- left_join(mc, tp, by = c("from" = "node"))
edges <- as.data.table(edges) %>% mutate(weight = probability*tput) %>% as.data.table()
edges <- edges[from != to & weight > 0, ]


edges[, fromStage := substr(from, 1, 2)]
edges[, toStage := substr(to, 1, 2)]
edges <- edges[toStage != "G1"]
edges <- edges[!(toStage %in% c("Mi", "Ma")),]
# 
# throughput <- copy(as.data.table(tp))
# rw=data.frame(tput = throughput[node=="Source", tput], node = "Sink")
# throughput <- rbind(throughput,rw)
# 


nodeAttrs <- as.data.table(tp)[node %in% unique(c(edges$to, edges$from)),] %>% 
  rbind(data.frame(tput = total_mis, node = "Mismanaged")) %>% 
  rbind(data.frame(tput = total_proper, node = "Managed")) %>%
  rename(label = node)
nodeAttrs <- nodeAttrs[!(label %in% c("Mismanaged", "Managed")),]
nodeAttrs <- nodeAttrs[tput > 0,][, id := 1:(.N)]
edges <- edges[(from %in% nodeAttrs$label) & (to %in% nodeAttrs$label),]


nodeAttrs[, state := substr(label, 1, 2)]

#edgeSinks <- edges[to %in% c("Mismanaged", "Managed"), ]
#nodeSinks <- nodeAttrs[label %in% c("Mismanaged", "Managed"), ]

#edges <-  edges[!(to %in% c("Mismanaged", "Managed")), ]
#nodeAttrs <-  nodeAttrs[!(label %in% c("Mismanaged", "Managed")), ]


nodeAttrs[, State := ifelse(state == "G1", 1,
                        ifelse(state == "C1", 2, 
                          ifelse(state == "I1", 3,
                             ifelse(state == "G2", 4,
                                    ifelse(state == "C2", 5,
                                           ifelse(state == "G3", 6, 
                                              ifelse(state == "Ma", 7, 8)))))))]
      

nodeAttrs <- nodeAttrs[order(State, -tput)]
nodeAttrs[, nodeNum := 1:(.N), by = State]
nodeAttrs[, nSize:=ifelse(State %in% c(7, 8), 15, 1)]
nodeAttrs[, broadState := substr(state, 1, 1)]


nodeAttrs[State %in% c(2, 3, 5), nodeNum := 51-nodeNum]
nodeAttrs <- nodeAttrs[order(State, nodeNum)]

nodeAttrs <- select(nodeAttrs, c(names(nodeAttrs)[c(2, 1, 3:8)]))
edges[, broadTo := substr(toStage, 1, 1)]

#connections <- left_join(connections, select(attrs, State, node), by = c("to" = "node")) %>% filter(from != "Source")                  
full <- graph_from_data_frame(edges, vertices = nodeAttrs) %>% as_tbl_graph()                               
E(full)$weight <- edges$weight



# set_graph_style(plot_margin = margin(100, 100, 100, 100))
# pal <- RColorBrewer::brewer.pal(6, 'RdYlBu')
# pal2 <- c(pal[1:3], pal[5:6])
# pal3 <- RColorBrewer::brewer.pal(4, 'YlGnBu')
# l <- create_layout(small, layout = "fr")
# l$State <- factor(l$State, levels = c("Mismanaged (MM)", "Managed Properly (MP)", "Recycling (R1/R2)", "Imports (I)", "Regeneration (G2/G3)", "VWG (G1)"))



l <- create_layout(full, layout = "linear", circular = TRUE)
l[l$name %in% c("Mismanaged", "Managed"), "x"] = 0
l[l$name %in% c("Mismanaged", "Managed"), "y"] = c(.1, -.1)


#l = as.data.table(l)

pal <- RColorBrewer::brewer.pal(8, 'RdYlBu')
pal2 <- RColorBrewer::brewer.pal(8, 'Spectral')

#pal <- rev(pal)


realPal = c("#0b0157", "#25cf2e", "#fcc390") #R, G, I
ePal = c("#0b0157", "#25cf2e", "#") #R, G, I

ggraph(l) + 
  theme_graph(title_family = "Calibri", title_size = 30) + 

  # labs(title = "Markov chain structure, three countries") +
  theme(plot.margin=unit(c(.5, .5, .5, .5),"cm"), text = element_text(size = 25)) + 
  geom_node_point(aes(size = tput, color = broadState), show.legend = F) +

  
  geom_edge_arc(alpha = .5, aes(width = weight, color = broadTo), show.legend = F) +
  # geom_edge_diagonal(aes(color = as.factor(State)), show.legend = F, alpha = .7) +
  # geom_node_point(aes(color = State), size = 7) +
  # #  geom_mark_rect(aes(x, y, group = country), fill = "Blue", size = .05, alpha = .05, label.fill = "White", label.fontsize = 15, label.buffer = unit(.2, "cm"), con.type = "straight", con.border = "all", label.lineheight = 20) +
   scale_color_manual(values = realPal) +
   scale_edge_color_manual(values = ePal) 

  # scale_fill_manual(values = pal3) 














allE <- left_join(select(edges, from, to, weight), nodeAttrs, by = c("from" = "label"))
allE <- allE[order(State, nodeNum)]


library(circlize)
chordDiagram(select(allE, from, to, weight), annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))


circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 10) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)












edges <- left_join(mc, tp, by = c("from" = "node"))
edges <- as.data.table(edges) %>% mutate(weight = probability*tput) %>% as.data.table()
edges <- edges[from != to & weight > 0, ]


edges[, fromStage := substr(from, 1, 2)]
edges[, toStage := substr(to, 1, 2)]
edges <- edges[toStage != "G1"]
edges <- edges[(toStage %in% c("Mi", "Ma")),]
# 
# throughput <- copy(as.data.table(tp))
# rw=data.frame(tput = throughput[node=="Source", tput], node = "Sink")
# throughput <- rbind(throughput,rw)
# 


nodeAttrs <- as.data.table(tp)[node %in% unique(c(edges$to, edges$from)),] %>% 
  rbind(data.frame(tput = total_mis, node = "Mismanaged")) %>% 
  rbind(data.frame(tput = total_proper, node = "Managed")) %>%
  rename(label = node)
#nodeAttrs <- nodeAttrs[!(label %in% c("Mismanaged", "Managed")),]
nodeAttrs <- nodeAttrs[tput > 0,][, id := 1:(.N)]
edges <- edges[(from %in% nodeAttrs$label) & (to %in% nodeAttrs$label),]


nodeAttrs[, state := substr(label, 1, 2)]

#edgeSinks <- edges[to %in% c("Mismanaged", "Managed"), ]
#nodeSinks <- nodeAttrs[label %in% c("Mismanaged", "Managed"), ]

#edges <-  edges[!(to %in% c("Mismanaged", "Managed")), ]
#nodeAttrs <-  nodeAttrs[!(label %in% c("Mismanaged", "Managed")), ]


nodeAttrs[, State := ifelse(state == "G1", 1,
                            ifelse(state == "C1", 2, 
                                   ifelse(state == "I1", 3,
                                          ifelse(state == "G2", 4,
                                                 ifelse(state == "C2", 5,
                                                        ifelse(state == "G3", 6, 
                                                               ifelse(state == "Ma", 7, 8)))))))]


nodeAttrs <- nodeAttrs[order(State, -tput)]
nodeAttrs[, nodeNum := 1:(.N), by = State]
nodeAttrs[, nSize:=ifelse(State %in% c(7, 8), 15, 1)]
nodeAttrs[, broadState := substr(state, 1, 1)]


nodeAttrs[State %in% c(2, 3, 5), nodeNum := 51-nodeNum]
nodeAttrs <- nodeAttrs[order(State, nodeNum)]

nodeAttrs <- select(nodeAttrs, c(names(nodeAttrs)[c(2, 1, 3:8)]))
edges[, broadTo := substr(toStage, 1, 1)]

#connections <- left_join(connections, select(attrs, State, node), by = c("to" = "node")) %>% filter(from != "Source")                  
full <- graph_from_data_frame(edges, vertices = nodeAttrs) %>% as_tbl_graph()                               
E(full)$weight <- edges$weight



# set_graph_style(plot_margin = margin(100, 100, 100, 100))
# pal <- RColorBrewer::brewer.pal(6, 'RdYlBu')
# pal2 <- c(pal[1:3], pal[5:6])
# pal3 <- RColorBrewer::brewer.pal(4, 'YlGnBu')
# l <- create_layout(small, layout = "fr")
# l$State <- factor(l$State, levels = c("Mismanaged (MM)", "Managed Properly (MP)", "Recycling (R1/R2)", "Imports (I)", "Regeneration (G2/G3)", "VWG (G1)"))



l <- create_layout(full, layout = "linear", circular = TRUE)
l[l$name %in% c("Mismanaged", "Managed"), "x"] = 0
l[l$name %in% c("Mismanaged", "Managed"), "y"] = c(.9, -.9)


#l = as.data.table(l)

pal <- RColorBrewer::brewer.pal(8, 'RdYlBu')
pal2 <- RColorBrewer::brewer.pal(8, 'Spectral')

#pal <- rev(pal)


realPal = c("#fa6e0a", "#bababa", "#bababa", "#bababa") #R, G, I
ePal = c("#2be813", "#e3e6e3", "#bababa", "#e3e6e3") #R, G, I

ggraph(l) + 
  theme_graph(title_family = "Calibri", title_size = 30) + 
  
  # labs(title = "Markov chain structure, three countries") +
  theme(plot.margin=unit(c(.5, .5, .5, .5),"cm"), text = element_text(size = 25)) + 
  geom_node_point(aes(size = tput, color = broadState), show.legend = F) +
  
  
  geom_edge_diagonal(alpha = .35, aes(width = weight, color = broadTo), show.legend = F) +
  # geom_edge_diagonal(aes(color = as.factor(State)), show.legend = F, alpha = .7) +
  # geom_node_point(aes(color = State), size = 7) +
  # #  geom_mark_rect(aes(x, y, group = country), fill = "Blue", size = .05, alpha = .05, label.fill = "White", label.fontsize = 15, label.buffer = unit(.2, "cm"), con.type = "straight", con.border = "all", label.lineheight = 20) +
  scale_color_manual(values = realPal) +
  scale_edge_color_manual(values = ePal) 

# scale_fill_manual(values = pal3) 











