library(tidyverse)
library(ggplot2)
library(markovchain)
library(stringr)

######create the markov chain

countries <- c("USA", "CHN", "MEX")
processes <- c("G1","G2", "G3", "C1", "C2", "I", "Managed", "Mismanaged")
t = length(processes)^2 + length(processes)

transitions <- matrix(rep(0, times = t), nrow = length(processes)) %>% as.data.frame()
transitions[,1] <- processes
names(transitions) <- c("from", processes)
transitions <- transitions %>% gather(key = "to", value = "probability", 2:(length(processes)+1))
transitions$country <- character(length(transitions[,1]))

rates <- data.frame(country = countries,
                    C1_rate = c(.3, .4, .3),
                    C2_rate = c(.8, .7, .6),
                    mismanagement_rate = c(.2, .2, .3), 
                    I_C2 = c(.3, .2, .4),
                    I_mismanagement = c(.3, .2, .4),
                    sorting_yield_rate = c(.7, .8, .6),
                    fraction_recycling_chemical = c(.1, .2, .3),
                    mechanical_yield_rate = c(.3, .2, .4),
                    chemical_yield_rate = c(.8, .9, .7),
                    trade_fraction = c(.5, .2, .4))

sorting_improvement_chemical = .2

#### dummy trade data
trade_data <- matrix(rep(0, times = length(countries)^2 + length(countries)), nrow = (length(countries))) %>% as.data.frame()
trade_data[,1] <- countries
names(trade_data) <- c("from", countries)
trade_data <- gather(trade_data, key = "to", value = "quantity", 2:(length(countries) + 1))
trade_data$quantity <- c(0, 2, 5, 4, 0, 5, 4, 3, 0)

# convert quantities to probabilities
export_totals <- trade_data %>%
  group_by(from) %>%
  summarize(exports = sum(quantity))
trade_data <- left_join(trade_data, export_totals) %>%
  mutate(probability = quantity/exports) %>%
  select(-quantity, -exports)
trade_data[trade_data$from == rates$country, 3] <- trade_data$probability * rates$trade_fraction


# create international transition table
all_transitions <- data.frame(from = character(0), 
                              to = character(0), 
                              probability = numeric(0), 
                              country = character(0))

for (c in countries){
  transitions$country <- c
  transitions$probability <- 0
  r <- rates[rates$country == c,]
  
  transitions[transitions$from == "G1" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate * (1 - r$C1_rate) * (1 - r$trade_fraction)
  transitions[transitions$from == "G1" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$C1_rate) * (1 - r$trade_fraction)
  transitions[transitions$from == "G1" & transitions$to == "C1", 3] <- r$C1_rate * (1 - r$trade_fraction)
  
  transitions[transitions$from == "G2" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate * (1 - r$C2_rate) * (1 - r$trade_fraction)
  transitions[transitions$from == "G2" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate) * (1 - r$C2_rate) * (1 - r$trade_fraction)
  transitions[transitions$from == "G2" & transitions$to == "C2", 3] <- r$C2_rate * (1 - r$trade_fraction)
  
  transitions[transitions$from == "G3" & transitions$to == "Managed", 3] <- (1 - r$mismanagement_rate)  * (1 - r$trade_fraction)
  transitions[transitions$from == "G3" & transitions$to == "Mismanaged", 3] <- r$mismanagement_rate  * (1 - r$trade_fraction)
  
  transitions[transitions$from == "C1" & transitions$to == "G1", 3] <- r$sorting_yield_rate * r$fraction_recycling_chemical * r$chemical_yield_rate
  transitions[transitions$from == "C1" & transitions$to == "G2", 3] <- r$sorting_yield_rate * (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
  C1_disposed = 1 - sum(transitions[transitions$from == "C1", "probability"])
  
  transitions[transitions$from == "C1" & transitions$to == "Managed", 3] <- C1_disposed * (1 - r$mismanagement_rate)
  transitions[transitions$from == "C1" & transitions$to == "Mismanaged", 3] <- C1_disposed * r$mismanagement_rate
  
  transitions[transitions$from == "C2" & transitions$to == "G1", 3] <- max(r$sorting_yield_rate + sorting_improvement_chemical, 1) * r$fraction_recycling_chemical * r$chemical_yield_rate
  transitions[transitions$from == "C2" & transitions$to == "G3", 3] <- r$sorting_yield_rate * (1 - r$fraction_recycling_chemical) * r$mechanical_yield_rate
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


##### concatenates processing state and country code in from and to columns
all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$from <- paste(all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$from, all_transitions[!(all_transitions$from %in% c("Managed", "Mismanaged")),]$country, sep = "_")
all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$to <- paste(all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$to, all_transitions[!(all_transitions$to %in% c("Managed", "Mismanaged")),]$country, sep = "_")
all_transitions <- select(all_transitions, -country)

##### appends trade data
trade_data1 <- trade_data
trade_data2 <- trade_data
trade_data3 <- trade_data
trade_data1$from <- paste("G1_", trade_data$from, sep = "")
trade_data1$to <- paste("I_", trade_data$to, sep = "")
trade_data2$from <- paste("G2_", trade_data$from, sep = "")
trade_data2$to <- paste("I_", trade_data$to, sep = "")
trade_data3$from <- paste("G3_", trade_data$from, sep = "")
trade_data3$to <- paste("I_", trade_data$to, sep = "")

all_transitions <- rbind(all_transitions, trade_data1, trade_data2, trade_data3) #append trade data



all_transitions <- unique(all_transitions) #remove duplicate sink states

probability_check <- all_transitions %>%
  group_by(from) %>%
  summarize(total_prob = sum(probability))

P <- spread(all_transitions, key = to, value = probability)
P[is.na(P)] = 0
rownames(P) <- P[,1]
P <- P[,-1]

apply(P, 1, sum)
P <- as.matrix(P)

mc <- new("markovchain", states = colnames(P), transitionMatrix = P)
################ calculating virgin plastic waste generation rates

generation_total = data.frame(country = countries, qty = c(1000, 1200, 600)) %>% arrange(country)
rhs <- generation_total$qty
H <- hittingProbabilities(mc)
H <- H["U1" == substr(rownames(H), 1, 2), "U2" == substr(colnames(H), 1, 2)]
diag(H) <- diag(H) + 1
virgin_waste <- solve(t(H), rhs)


############ metrics
chain <- new("markovchain", states = rownames(P), transitionMatrix = P)
absorbing <- absorbingStates(chain)
transient <- transientStates(chain)
absorptionPr <- absorptionProbabilities(chain)
absorptionTime <- meanAbsorptionTime(chain)
hittingPr <- hittingProbabilities(chain)


absorbing
transient
absorptionPr #filter to show U1 to absorbing states
absorptionTime #filter to only show U1 to C
hittingPr


test <- matrix(rep(c(rep(0, 1000), 1), 1001), byrow = T, nrow = 1001)
names(test) <- as.character(1:1001)
rownames(test) <- as.character(1:1001)

testChain <- new("markovchain", states = as.character(1:1001), transitionMatrix = test)

############################# markov chain functions
S = 2000
states <- as.character(seq(1, S, by = 1))
a <- matrix(runif(S^2, 0, 1), nrow = S)

#make the matrix more sparse. 
for(i in 1:S^2){
  if(runif(1, 0, 1) < .9){
    a[ceiling(i/S), i %% (S + 1)] = 0
  }
}
a <- a / apply(a, 1, sum)
rownames(a) <- states
colnames(a) <- states

#converts a to an absorbing markov chain
a[(S - 1):S,] <- 0
a[(S-1), (S-1)] = 1
a[S, S] = 1

chain <- new("markovchain", states = states, transitionMatrix = a)
names(chain)
states(chain)
print(chain)
plot(chain)
summary(chain)
is.irreducible(chain)
is.accessible(chain, from = "0", to = "2")
communicatingClasses(chain)
steadyStates(chain)
transientStates(chain)
absorbingStates(chain)
meanFirstPassageTime(chain)
meanAbsorptionTime(chain)
hittingProbabilities(chain)
absorptionProbabilities(chain)

initial = (1:10)
hits = 75

#package function speed
t = Sys.time()
chain <- new("markovchain", states = states, transitionMatrix = a)
hittingProbabilities(chain)[initial, hits]
print(Sys.time() - t)

#manual speed
t = Sys.time()
Q <- a[-c(hits, S-1, S), -c(hits, S-1, S)] 
C <- -a[-c(hits, S-1, S), hits]
solve(Q - diag(sqrt(length(Q))), C)[initial]
print(Sys.time() - t)


