library(tidyverse)


ps <- function(x1, x2, x3){
  lp1 <- 0.05*x1 - x2 - x3
  lp2 <- 0.5 - 0.05*x1 + x2 + x3
  pr1 <- exp(lp1)/ (1 + exp(lp1) + exp(lp2))
  pr2 <- exp(lp2)/ (1 + exp(lp1) + exp(lp2))
  pr0 <- 1 - pr1 - pr2
  return(c(pr0, pr1, pr2))
}

y_cost <- function(x1, x2, x3, tx){
  y_exp <- 
    10 + x1 + x2*15 + 15*x3 + 
    50*(tx == 1) + 100*(tx == 2)
  rlnorm(1, mean = log(y_exp))
}

create_data <- function(sample_size = 500){
  data <- tibble(
    x1 = rgamma(sample_size, shape = 8, rate = 0.25), 
    x2 = rbernoulli(sample_size), 
    x3 = rbeta(sample_size, 3, 1.5), 
    tx = pmap_int(list(x1, x2, x3), ~ sample(0:2, 1, prob = ps(..1,..2,..3))),
    y_obs = pmap_dbl(list(x1, x2, x3, tx), y_cost)
  )
  return(data)
}

glm(y_obs ~ tx + x1 + x2 + x3, data = sim_data, family = gaussian(link = "log"))

ps_rf <- randomForest::randomForest(tx ~ x1 + x2 + x3, data = sim_data)
predict(ps_rf, sim_data, type = 'prob')

X = as.matrix(select(sim_data, x1:x3) %>% 
                mutate(x2 = as.numeric(as.logical(x2)))
              )

library(multilevelMatching)
set.seed(123)
fit <- multiMatch(
  Y = sim_data$y_obs,
  W = as.numeric(as.character(sim_data$tx)),
  X = X,
  match_on = "covariates"
)


fit


ce_rf <- randomForest::randomForest(y_obs ~ tx + x1 + x2 + x3, data = sim_data)
predict(ce_rf, mutate(sim_data, tx = factor(2, levels = 0:2))) %>% 
  mean(.) - predict(ce_rf, mutate(sim_data, tx = factor(0, levels = 0:2))) %>% 
  mean(.)
