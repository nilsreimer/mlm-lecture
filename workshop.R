rm(list = ls())

# Instructions ------------------------------------------------------------

  #########################################################################
  # Please run the script until "Prepare data" before attempting anything #
  # else. Code for "Model 0" and "Model 1" is provided; please fill in    #
  # code for "Model 2" and "Model 3". Questions at the end of each        #
  # section help guide your research reports.                             # 
  #########################################################################


# Install packages --------------------------------------------------------
  
  #########################################################################
  # If packages are not installed, un-comment and run the                 # 
  # install.packages(...) commands below.                                 #
  #########################################################################

  # install.packages("lme4")
  # install.packages("tidyverse")
  # install.packages("skimr")
  # install.packages("arm")


# Load packages -----------------------------------------------------------
  library(tidyverse); library(lme4)


# Load functions ----------------------------------------------------------
  
  #########################################################################
  # I have prepared some helpful functions (standardise, plot_intercepts, #
  # plot_slopes) for you. See functions.R for details.                    #
  #########################################################################
  
  source("functions.R")


# Prepare data ------------------------------------------------------------
  
  #########################################################################
  # In this section, you import, inspect, and standardise the dataset.    #
  #########################################################################

  # Import data
  d <- read_rds("data/d_wk6.rds")
  
  # Inspect data
  print(d, n = 10)
  glimpse(d)
  skimr::skim(d)
  count(d, jj)
  
  # Standardise (Level 1: Participants)
  d_l1 <- d %>% 
    select(ii, jj, contact, attitudes) %>%
    mutate(
      contact = standardise(contact),
      attitudes = standardise(attitudes)
    )
  
  # Standardise (Level 2: Neighbourhoods)
  d_l2 <- d %>% 
    distinct(jj, diversity) %>%
    mutate(
      diversity = standardise(diversity)
    )
  
  # Merge levels
  d_z <- left_join(d_l1, d_l2, by = "jj")
  
  # Inspect data
  print(d_z, n = 10)
  skimr::skim(d_z)


# Model 0 -----------------------------------------------------------------

  #########################################################################
  # In this section, you estimate a null model with a varying (random)    #
  # intercept. You inspect the model coefficients, you calculate the ICC, #
  # and you plot the varying (random) intercepts.                         #
  #########################################################################
  
  # Estimate multilevel model
  m0 <- lmer(attitudes ~ 1 + (1|jj), data = d_z) 
  
  # Inspect model
  summary(m0)                      # Inspect model parameters
  confint(m0, method = "boot")     # Compute bootstrap confidence intervals
  
  # Calculate intraclass correlation coefficient (ICC)
  sds          <- as_tibble(VarCorr(m0))
  icc_jj       <- sds$vcov[1] / ( sds$vcov[1] + sds$vcov[2] )
  icc_residual <- sds$vcov[2] / ( sds$vcov[1] + sds$vcov[2] )
  
  # Plot varying (random) intercepts
  plot_intercepts(m0)


# Model 1 -----------------------------------------------------------------

  #########################################################################
  # In this section, you estimate to what extent neighbourhood diversity  #
  # is associated with more or less favourable attitudes. First, you try  #
  # estimating the effect of diversity in a simple regression model.      #
  #########################################################################
  
  # Estimate simple regression model
  lm(attitudes ~ diversity, data = d_z) %>% summary()
  
  # Aggregate data
  d_jj <- d_z %>% group_by(jj) %>% summarise_at(vars(-jj), mean) %>% select(-ii)
  print(d_jj, n = 30)
  
  # Estimate simple regression model on aggregated data
  lm(attitudes ~ diversity, data = d_jj) %>% summary()
  
  #########################################################################
  # What did you learn from these analyses? Is linear regression a valid  #
  # method to estimate the effects of neighbourhood diversity? Next, you  #
  # estimate a multilevel model, regressing attitudes on neighbourhood    #
  # diversity.                                                            #
  #########################################################################
  
  # Estimate multilevel model
  m1 <- lmer(attitudes ~ 1 + diversity + (1|jj), data = d_z)

  # Inspect model
  summary(m1)                      # Inspect model parameters
  confint(m1, method = "boot")     # Compute bootstrap confidence intervals
  
  # Calculate intraclass correlation coefficient (ICC)
  sds          <- as_tibble(VarCorr(m1))
  icc_jj       <- sds$vcov[1] / ( sds$vcov[1] + sds$vcov[2] )
  icc_residual <- sds$vcov[2] / ( sds$vcov[1] + sds$vcov[2] )
  
  # Plot varying (random) intercepts
  plot_intercepts(m1)
  
  # Compare models
  anova(m0, m1)
  
  #########################################################################
  # What did you learn from these analyses? Is neighbourhood diversity    #
  # associated with less favourable attitudes?                            #
  #########################################################################
  

# Model 2 -----------------------------------------------------------------
  
  #########################################################################
  # In this section, you estimate to what extent neighbourhood diversity  #
  # and intergroup contact are associated with attitudes. Use what you    #
  # know about the data and about the lmer function to fill in the code.  #
  #########################################################################
  
  # Estimate multilevel model
  m2 <- lmer(..., data = d_z)

  # Inspect model

  
  # Calculate intraclass correlation coefficient (ICC)

  
  # Plot varying (random) intercepts
  plot_intercepts(m2)
  
  # Compare models
  anova(m0, m1, m2)
  
  #########################################################################
  # What do you conclude? Is intergroup contact associated with more or   #
  # less favourable attitudes? Has controlling for individual conact      #
  # experiences changed the coefficient for diversity? How would you      # 
  # interpret this finding? Did including an additional predictor change  #
  # the residual variance and/or neighbourhood-level variance? Why?       #
  #########################################################################


# Model 3 -----------------------------------------------------------------
  
  #########################################################################
  # In this section, you estimate to what extent the relationship between #
  # contact and attitudes varies across neighbourhoods, using a mode with #
  # varying (random) slopes.                                              #
  #########################################################################
  
  # Estimate multilevel model
  m3 <- lmer(..., data = d_z)
  
  # Inspect model
  

  #########################################################################
  # Note: The intraclass correlation coefficient, as you have calculated  #
  # it thus far, is meaningless for varying-slopes models.                #
  #########################################################################
  
  # Plot varying (random) intercepts
  plot_intercepts(m3)
  
  # Plot varying (random) slopes
  plot_slopes(m3)  
  
  # Compare models
  anova(m0, m1, m2, m3)
  
  #########################################################################
  # How do you interpret these findings? What does the standard deviation #
  # of the slope mean? What does it mean that the varying intercept and   #
  # slope are negatively correlated? Can you think of an explanation?     #
  #########################################################################

  
# Revision ----------------------------------------------------------------

  #########################################################################
  # Check how much you remember from last week's class (on data           #
  # visualisation) and try to plot the relationships between contact,     #
  # diversity, and attitudes.                                             #
  #########################################################################
  