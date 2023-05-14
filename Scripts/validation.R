library(tidyverse)
library(markovchain)
library(expm) 
library(ggplot2)

P <- matrix(c(.2, .1, .3, .35, 0, .05, 
              .4, .3, .1, .15, .05, 0, 
              0, .1, .05, .7, .1, .05, 
              0, 0, .2, .3, .1, .4,  
              0, 0, 0, 0, 1, 0, 
              0, 0, 0, 0, 0, 1), byrow = T, nrow = 6)
P
apply(P, 1, sum) #check that rows sum to 1

dtmc <- new("markovchain", states = c("1", "2", "3", "4", "5", "6"), transitionMatrix = P)
transientStates(dtmc)
absorbingStates(dtmc)

i = 1
k = 3
j = 5

initial = i
intermediate = k
absorbing = j
time = 2
markovChain = dtmc

timeDensity <- function(markovChain, initial, intermediate, absorbing, time){
  pij_t <- (markovChain[] %^% time)[as.numeric(initial), as.numeric(absorbing)]
  pij_t1 <- (markovChain[] %^% (time-1))[as.numeric(initial), as.numeric(absorbing)]
  aij = absorptionProbabilities(markovChain)[initial, absorbing]
  
  time_prob = (pij_t - pij_t1)/aij
  return(time_prob)
}

M = 26

probability_not = 0
for(y in 2:M){
  Psj <- P 
  Psj[,as.numeric(absorbingStates(dtmc))] <- 0
  Psk <- Psj
  Psk[, as.numeric(k)] <- 0
  
  term1 <- (round(Psk %^% (y-1), digits = 6) %*% P)[as.numeric(i), as.numeric(j)]/(round(Psj %^% (y-1), digits = 6) %*% P)[as.numeric(i), as.numeric(j)] %>% round(5)
  term2 <- timeDensity(markovChain = dtmc, initial = i, intermediate =  k, absorbing = j, time = y) %>% round(5)
  probability_not = probability_not + (term1*term2)
}

probability <- 1 - probability_not
probability
Psk_norm <- Psk
Psk_norm[1:4, 1:4] <- Psk[1:4, 1:4]/(apply(Psk[1:4, 1:4], 1, sum))
apply(Psk_norm, 1, sum)
Psk_norm[5:6, 5:6] <- matrix(c(1, 0, 0, 1), byrow = T, nrow = 2)
Psk <- Psk_norm

Psj_norm <- Psj


##### random walk simulation
results <- data.frame(iteration = integer(0), 
                      hits_j = logical(0),
                      hits_k = logical(0))

theta <- data.frame(iter = integer(0), 
                    prob = double(0))
for(j in 1:50){
  m = 1000
  for(i in 1:m){
    sequence <- rmarkovchain(n = 10, dtmc, t0 = "1")
    results_i <- data.frame(iteration = i, hits_k = (k %in% sequence), hits_j = (j %in% sequence))
    results <- rbind(results, results_i)
  }
  exp_prob = length(filter(results, hits_j == T, hits_k == T)[,1]) / length(filter(results, hits_j == T)[,1])
  theta <- rbind(theta, c(j, exp_prob))
}

exp_prob = length(filter(results, hits_j == T, hits_k == T)[,1]) / length(filter(results, hits_j == T)[,1])
names(theta)[2] <- "prob"
ggplot(theta) + geom_density(aes(x = prob))
var(theta$prob)
mean(theta$prob)

#### simplified approach
M = 50000

probability_not = 0
P_prime <- P
P_prime[, as.numeric(k)] <- 0
P_prime[,as.numeric(absorbingStates(dtmc))] <- 0
for(y in 2:M){
  probability_not = probability_not + ((P_prime %^% (y-1)) %*% P)[i, j]/absorptionProbabilities(dtmc)[i, 1]
}



probability <- 1 - probability_not
probability







############## reference
chain <- new("markovchain", states = c("0","1","2","3"), transitionMatrix = a)
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

steadyStates(chain)[3]/(1-steadyStates(chain)[1])