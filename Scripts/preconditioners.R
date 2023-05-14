library(tidyverse)
library(Rlinsolve)
library(Matrix)


countries <- read_csv("Data/regions.csv")$iso3c
countries <- countries[!is.na(countries)][1:50]
processes <- c("G1","G2", "G3", "C1", "C2", "I", "Managed", "Mismanaged")
t = length(processes)^2 + length(processes)

transitions_template <- matrix(rep(0, times = t), nrow = length(processes)) %>% as.data.frame()
transitions_template[,1] <- processes
names(transitions_template) <- c("from", processes)
transitions_template <- transitions_template %>% gather(key = "to", value = "probability", 2:(length(processes)+1))
transitions_template$country <- character(length(transitions_template[,1]))

rates <- read_csv("Data/rates.csv")
rates$I_mismanagement <- rates$mismanagement_rate
rates$I_C2 <- rates$C2_rate
rates <- rates[rates$country %in% countries,]


# randomly generate dummy trade data
trade_data <- matrix(rep(0, times = length(countries)^2 + length(countries)), nrow = (length(countries))) %>% as.data.frame()
trade_data[,1] <- countries
names(trade_data) <- c("from", countries)
trade_data <- gather(trade_data, key = "to", value = "quantity", 2:(length(countries) + 1))
trade_data$quantity <- runif(length(countries)^2, 0, 1)*100
trade_data[trade_data$from == trade_data$to, 3] <- 0

# convert quantities to probabilities
export_totals <- trade_data %>%
  group_by(from) %>%
  summarize(exports = sum(quantity))
trade_data <- left_join(trade_data, export_totals) %>%
  mutate(probability = quantity/exports) %>%
  select(-quantity, -exports)
trade_data[trade_data$from == rates$country, 3] <- trade_data$probability * rates$trade_fraction


rates$mechanical_yield_rate <- runif(n = length(countries), 0.25, 0.75)
rates$chemical_yield_rate <- runif(n = length(countries), 0.25, 0.75)


######################### create the markov chain #########################
transitions <- transitions_template
all_transitions <- data.frame(from = character(0), 
                              to = character(0), 
                              probability = numeric(0), 
                              country = character(0))
for (country in countries){
  transitions$country <- country
  transitions$probability <- 0
  r <- rates[rates$country == country,]
  
  transitions[transitions$from == "G1" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate * (1 - r$trade_fraction - (1 - r$trade_fraction) * r$C1_rate)
  transitions[transitions$from == "G1" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$trade_fraction - (1 - r$trade_fraction) * r$C1_rate)
  transitions[transitions$from == "G1" & transitions$to == "C1", 3] <- r$C1_rate * (1 - r$trade_fraction)
  
  transitions[transitions$from == "G2" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate * (1 - r$trade_fraction - (1 - r$trade_fraction) * r$C2_rate)
  transitions[transitions$from == "G2" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$trade_fraction - (1 - r$trade_fraction) * r$C2_rate)
  transitions[transitions$from == "G2" & transitions$to == "C2", 3] <- r$C2_rate * (1 - r$trade_fraction)
  
  transitions[transitions$from == "G3" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate)  * (1 - r$trade_fraction)
  transitions[transitions$from == "G3" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate  * (1 - r$trade_fraction)
  
  transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
  transitions[transitions$from == "C1" & transitions$to == "G2", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
  C1_disposed = 1 - sum(transitions[transitions$from == "C1", "probability"])
  
  transitions[transitions$from == "C1" & transitions$to == "Managed", 3] <- C1_disposed * (1 - r$mismanagement_rate)
  transitions[transitions$from == "C1" & transitions$to == "Mismanaged", 3] <- C1_disposed * r$mismanagement_rate
  
  transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- r$fraction_recycling_chemical * r$chemical_yield_rate
  transitions[transitions$from == "C2" & transitions$to == "G3", 3] <- (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
  C2_disposed = 1 - sum(transitions[transitions$from == "C2", "probability"])
  
  transitions[transitions$from == "C2" & transitions$to == "Managed", 3] <- C2_disposed * (1 - r$mismanagement_rate)
  transitions[transitions$from == "C2" & transitions$to == "Mismanaged", 3] <- C2_disposed * r$mismanagement_rate
  
  transitions[transitions$from == "I" & transitions$to == "C2", 3] <- r$I_C2
  transitions[transitions$from == "I" & transitions$to == "Mismanaged", 3] <- (1 - r$I_C2) * r$I_mismanagement
  transitions[transitions$from == "I" & transitions$to == "Managed", 3] <- (1 - r$I_C2) * (1 - r$I_mismanagement)
  
  transitions[transitions$from == "Mismanaged" & transitions$to == "Mismanaged", 3] <- 1
  transitions[transitions$from == "Managed" & transitions$to == "Managed", 3] <- 1
  all_transitions <- rbind(all_transitions, transitions)
}


# concatenates processing state and country code in from and to columns
all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$from <- paste(all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$from, all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$country, sep = "_")
all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$to <- paste(all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$to, all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$country, sep = "_")
all_transitions <- select(all_transitions, -country)

# appends trade data
trade_data1 <- trade_data
trade_data2 <- trade_data
trade_data3 <- trade_data
trade_data1$from <- paste("G1_", trade_data$from, sep = "")
trade_data1$to <- paste("I_", trade_data$to, sep = "")
trade_data2$from <- paste("G2_", trade_data$from, sep = "")
trade_data2$to <- paste("I_", trade_data$to, sep = "")
trade_data3$from <- paste("G3_", trade_data$from, sep = "")
trade_data3$to <- paste("I_", trade_data$to, sep = "")
all_transitions <- rbind(all_transitions, trade_data1, trade_data2, trade_data3) 
all_transitions <- unique(all_transitions) #remove duplicate sink states

# create the transition matrix from the data frame
P <- pivot_wider(all_transitions, names_from = to, values_from = probability) %>% as.data.frame()
P[is.na(P)] = 0
rownames(P) <- P[,1]
P <- P[,-1]
P <- as.matrix(P)

hits <- rownames(P)[substr(rownames(P), 1, 1) == "G" | substr(rownames(P), 1, 1) == "I" | substr(rownames(P), 1, 2) == "C1"]
absorbing <- c("Managed", "Mismanaged")
transitionMat <- P
targets <- hits

t = Sys.time()
hit_probs <- data.frame(source = rownames(transitionMat)[!(rownames(transitionMat) %in% absorbing)])
for(i in 1:length(targets)){
  
  rhs <- c(rep(0, times = ncol(transitionMat) - length(absorbing) - 1), 1)
  Q <- transitionMat[!(rownames(transitionMat) %in% c(targets[i], absorbing)), !(colnames(transitionMat) %in% c(targets[i], absorbing))]
  C <- transitionMat[!(rownames(transitionMat) %in% c(targets[i], absorbing)), colnames(transitionMat) == targets[i]]
  I <- diag(sqrt(length(Q)))
  W <- rbind(cbind((Q-I), C), rhs)
  
  #possible preconditioners
  #pre <- diag(diag(W), nrow = nrow(W), ncol = ncol(W))
  #pre <- W 
  #pre[upper.tri(pre, diag = FALSE)] <- 0
  lu <- lu(W)
  elu <- expand(lu)
  L <- elu$L
  U <- elu$U
  P <- elu$P
  pre <- L%*%U
  sol <- round(lsolve.bicgstab(A = W, B = rhs, maxiter = 1000, preconditioner = pre)$x, digits = 4) %>% as.data.frame()
  names(sol) = targets[i]
  source <- c(rownames(Q), targets[i])
  sol <- cbind(sol, source)
  hit_probs <- left_join(hit_probs, sol)
}
names(hit_probs)[2:ncol(hit_probs)] = targets
rownames(hit_probs) <- hit_probs[,1]
hit_probs <- hit_probs[,-1]
hit_probs <- hit_probs[order(rownames(hit_probs)), order(colnames(hit_probs))] %>% as.matrix()
print(Sys.time() - t)

