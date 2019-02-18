rm(list = ls())

# Load packages -----------------------------------------------------------
  library(lme4); library(tidyverse); library(skimr)


# Functions ---------------------------------------------------------------
  mvrnorm <- function(n, mu, sigma, names) {
    Y <- MASS::mvrnorm(
      n = n,
      mu = mu,
      Sigma = matrix(sigma, ncol = length(mu)),
      empirical = TRUE
    ) %>% as_tibble() %>% mutate(id = 1:n)
    names(Y) <- names
    return(Y)
  }
  standardise <- function(v) ( v - mean(v) ) / sd(v)
  rescale <- function(z, m, sd, l, u, d) {
    v <- z * sd + m
    v <- round(v, d)
    v <- if_else(v < l, l, v)
    v <- if_else(v > u, u, v)
    return(v)
  }


# Set parameters ----------------------------------------------------------
  
  # Sample
  N  <- 900                                  # n of participants
  J  <-  30                                  # n of neighbourhoods
  ii <- 1:N                                  # vector of participants
  jj <- sort(sample(1:J, N, replace = TRUE)) # vector of neighbourhoods
  
  # Fixed effects
  b_0         <-  0
  b_contact   <-  0.55
  b_diversity <- -0.45
  
  # Standard deviations
  s_residual   <- 1
  s_jj         <- 0.7
  s_jj_contact <- 0.3
  

# Simulate data -----------------------------------------------------------
  
  set.seed(7679090)
  
  # Varying effects
  e_ii         <- mvrnorm(N, c(0), c(s_residual^2), c("e_ii", "ii"))
  b_jj         <- mvrnorm(J, c(0, 0), c(s_jj^2, -0.084, -0.084, s_jj_contact^2), c("b_jj", "b_jj_contact", "jj"))

  # Predictors
  X_j <- mvrnorm(J, c(0, 0), c(1, 0.65, 0.65, 1), c("contact", "diversity", "jj")) %>%
         mutate_at(vars(contact, diversity), ~(. - mean(.))/sd(.))
  X_i <- tibble(ii, jj) %>% 
         mutate(
           contact = map_dbl(jj, ~rnorm(1, X_j$contact[.], 2)),
           contact = ( contact - mean(contact) ) / sd(contact)
         )
  
  # Outcome
  Y_i <- tibble(ii, jj) %>%
    left_join(X_i, by = c("ii", "jj")) %>%
    left_join(X_j %>% select(jj, diversity), by = "jj") %>%
    mutate(
      attitudes = (b_0 + b_jj$b_jj[jj]) + b_diversity*diversity + (b_contact + b_jj$b_jj_contact[jj])*contact + e_ii$e_ii[ii]
    )
  
  # Assess
  Y_i %>% select(-ii, -jj) %>% skim()
  Y_i %>% group_by(jj) %>% summarise_all(mean) %>% select(-ii, -jj) %>% skim()
  Y_i %>% select(-ii, -jj) %>% cor() %>% round(2)
  Y_i %>% group_by(jj) %>% summarise_all(mean) %>% select(-ii, -jj) %>% cor() %>% round(2)
  m0 <- lmer(attitudes ~ 1 + (1|jj), data = Y_i)
  m1 <- lmer(attitudes ~ 1 + diversity + (1|jj), data = Y_i)
  m2 <- lmer(attitudes ~ 1 + diversity + contact + (1|jj), data = Y_i)
  m3 <- lmer(attitudes ~ 1 + diversity + contact + (1 + contact|jj), data = Y_i)


# Transform ---------------------------------------------------------------
  
  # Standardise dataset
  d_z <- Y_i %>% mutate(attitudes = standardise(attitudes))
    
  # Rescale dataset
  d   <- d_z %>%
    mutate(
      contact = rescale(contact, 4, 1, 1, 7, 1),
      diversity = rescale(diversity, 20, 10, 0, 100, 1),
      attitudes = rescale(attitudes, 50, 15, 0, 100, 0) %>% as.integer()
    ) # %>% rename(prtcpnt = ii, neighbh = jj)
  
  
# Export ------------------------------------------------------------------
  write_rds(d, "data/d_wk6.rds")
  