library(tidyverse)
sample_size <- 500

simulate_pre_data <- function(static_var1, static_var2, static_var3){
  y_exp <- 10 + static_var1 + static_var2*15 + 15*static_var3
  l2 <- exp(-40 + static_var1 + 10*static_var2 + 5*static_var3)
  pre_data <- tibble(
    month = -1:-4, 
    present = c(1, rbinom(3, 1, prob = 0.9)), 
    cost1 = rlnorm(4, mean = log(y_exp)),
    cost2 = replicate(4, 
                      ifelse(
                        rbinom(1, size = 1, prob = 0.2) == 0, 
                        0, rlnorm(1, mean = log(rnorm(1, mean = y_exp+500))
                        )
                      )
    ),
    count1 = rpois(4, runif(1, max = 4)),
    count2 = replicate(4, 
                       ifelse(
                         rbinom(1, size = 1, prob = l2/(l2+1)) == 0, 
                         0, rpois(4, runif(1, max = 4))
                         )
                       )
  ) %>% mutate(present = cummin(present)) %>% filter(present == 1) %>% 
    select(-present)
  return(pre_data)
}

sim_data_with_time <- tibble(
  id = 1:sample_size,
  static_var1 = rgamma(sample_size, shape = 8, rate = 0.25), 
  static_var2 = rbernoulli(sample_size), 
  static_var3 = rbeta(sample_size, 3, 1.5)
) %>% 
  mutate(pre_data = pmap(select(., -id), .f = simulate_pre_data)) %>% 
  unnest(pre_data)

