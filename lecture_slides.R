rm(list = ls())


# Load packages -----------------------------------------------------------
  library(tidyverse); library(lme4); library(broom)


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
  export_png <- function(name, width = 21.91, height = 16.14) {
    ggsave(
      file = paste0("figures/", name, ".png"),
      width = width, height = height, units = "cm",
      type = "cairo-png", dpi = 600
    )
  }


# Figure 1 ----------------------------------------------------------------
  
  set.seed(9176097)
  
  # Simulate data
  d1 <- tibble(
      ii = 1:160,
      jj = rep(c("A", "B", "C", "D"), each = 40),
      x  = rnorm(160) %>% standardise() %>% rescale(62, 7, 50, 80, 0),
      e  = rnorm(160, 0, 2) 
    ) %>%
    mutate(
      y = case_when(
        jj == "A" ~  0 + 0.60 * x + e,
        jj == "B" ~ 20 + 0.60 * x + e,
        jj == "C" ~ 10 + 0.60 * x + e,
        jj == "D" ~  5 + 0.60 * x + e
      )
    )
  
  # 2 Groups, Varying Intercepts 
  d1 %>%
    filter(jj %in% c("A", "B")) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(0, 20),
      slope = c(0.60, 0.60),
      colour = c("#D81B60", "#1E88E5"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-1a")
  
  # 3 Groups, Varying Intercepts 
  d1 %>%
    filter(jj %in% c("A", "B", "C")) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(0, 10, 20),
      slope = c(0.60, 0.60, 0.60),
      colour = c("#D81B60", "#FFC107", "#1E88E5"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-1b")
  
  # 4 Groups, Varying Intercepts 
  d1 %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(0, 10, 20, 5),
      slope = c(0.60, 0.60, 0.60, 0.60),
      colour = c("#D81B60", "#FFC107", "#1E88E5", "#004D40"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-1c")

    
# Figure 2 ----------------------------------------------------------------
  
  set.seed(9176097)
  
  # Simulate data
  d2 <- tibble(
    ii = 1:160,
    jj = rep(c("A", "B", "C", "D"), each = 40),
    x  = rnorm(160) %>% standardise() %>% rescale(62, 7, 50, 80, 0),
    e  = rnorm(160, 0, 2) 
  ) %>%
    mutate(
      y = case_when(
        jj == "A" ~ 30 + 1.20 * (x-50) + e,
        jj == "B" ~ 50 + 0.20 * (x-50) + e,
        jj == "C" ~ 40 + 0.60 * (x-50) + e,
        jj == "D" ~ 35 + 0.80 * (x-50) + e
      )
    )
  
  # 2 Groups, Varying Slopes
  d2 %>%
    filter(jj %in% c("A", "B")) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(30-50*1.2, 50-50*0.2),
      slope = c(1.20, 0.20),
      colour = c("#D81B60", "#1E88E5"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-2a")
  
  # 3 Groups, Varying Slopes
  d2 %>%
    filter(jj %in% c("A", "B", "C")) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(30-50*1.2, 50-50*0.2, 40-50*0.6),
      slope = c(1.20, 0.20, 0.60),
      colour = c("#D81B60", "#1E88E5", "#FFC107"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-2b")
  
  # 4 Groups, Varying Slopes
  d2 %>%
    filter(jj %in% c("A", "B", "C", "D")) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point(aes(shape = jj), size = 2) +
    geom_abline(
      intercept = c(30-50*1.2, 50-50*0.2, 40-50*0.6, 35-50*0.8),
      slope = c(1.20, 0.20, 0.60, 0.80),
      colour = c("#D81B60", "#1E88E5", "#FFC107", "#004D40"),
      size = 1
    ) +
    scale_shape(guide = "none") +
    coord_fixed(25/45*1.5, xlim = c(50, 75), ylim = c(25, 70)) +
    labs(
      x = "Achievement",
      y = "Income"
    ) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank())
  
  export_png("figure-2d")

  
# Figure 3 ----------------------------------------------------------------
  
  set.seed(1962392)
  
  # Simulate data
  d3 <- left_join(
    crossing(ii = 1:30, jj = 1:10),
    mvrnorm(10, c(0, 0), c(1, 0.65, 0.65, 1), c("diversity", "attitudes", "jj")),
    by = "jj"
  ) %>%
  mutate(
    attitudes = standardise(attitudes + rnorm(n(), 0, 2)) %>% 
                rescale(., 50, 15, 0, 100, 0),
    diversity = standardise(diversity) %>%
                rescale(., 20, 10, 0, 100, 1)
  )

  # False
  ggplot(d3, aes(x = diversity, y = attitudes)) +
    geom_point(colour = "grey70", size = 2) +
    geom_smooth(method = "lm", colour = "#1E88E5", alpha = 0.5, size = 1) +
    coord_fixed(35/100, xlim = c(0, 35), ylim = c(0, 100)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Diversity (Neighbourhood)",
      y = "Attitudes (Participant)"
    )
  
  export_png("figure-3a")
  
  
  # True
  d3 %>%
    group_by(jj) %>%
    summarise_all(mean) %>%
  ggplot(., aes(x = diversity, y = attitudes)) +
    geom_point(data = d3, colour = "grey90", size = 2) +
    geom_smooth(data = d3, method = "lm", colour = NA, alpha = 0.5, size = 1) +
    geom_smooth(method = "lm", colour = "#1E88E5", size = 1) +
    geom_point(shape = "diamond", colour = "#1E88E5", size = 3) +
    coord_fixed(35/100, xlim = c(0, 35), ylim = c(0, 100)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Diversity (Neighbourhood)",
      y = "Attitudes (Participant)"
    )
  
  export_png("figure-3b")
  

# Figure 4 ----------------------------------------------------------------
  
  set.seed(4292886)
  
  # Simulate data
  d4 <- crossing(
    ii = 1:20,
    jj = 1:8
  ) %>%
  left_join(
    tibble(jj = 1:8, d_jj = rnorm(8, 0.25, 0.05)),
    by = "jj"
  ) %>%
  mutate(
    d_ii = rnorm(n(), d_jj, 0.3)
  )
  
  # No pooling
  ggplot(d4, aes(x = ordered(jj), y = d_ii)) +
    stat_summary(fun.data = "mean_cl_normal", shape = 16, size = 0.75) +
    geom_hline(yintercept = 0.25, linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(9/0.75, xlim = c(1, 8), ylim = c(-0.25, 0.50)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Condition",
      y = expression(italic(d)),
      title = "No Pooling"
    )
  
  export_png("figure-4a", width = 21.91/2)
  
  # Complete pooling
  crossing(jj = 1:8, d_ii = d4$d_ii) %>%
  ggplot(., aes(x = ordered(jj), y = d_ii)) +
    stat_summary(fun.data = "mean_cl_normal", shape = 16, size = 0.75) +
    geom_hline(yintercept = 0.25, linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(9/0.75, xlim = c(1, 8), ylim = c(-0.25, 0.50)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Condition",
      y = expression(italic(d)),
      title = "Complete Pooling"
    )
  
  export_png("figure-4b", width = 21.91/2)
  
  # No pooling (null)
  ggplot(d4, aes(x = ordered(jj), y = -1*(d_ii - mean(d_ii)))) +
    stat_summary(fun.data = "mean_cl_normal", shape = 16, size = 0.75) +
    geom_hline(yintercept = 0.25, linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(9/0.75, xlim = c(1, 8), ylim = c(-0.25, 0.50)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Condition",
      y = expression(italic(d)),
      title = "No Pooling"
    )
  
  export_png("figure-4c", width = 21.91/2)
  
  # Complete pooling (null)
  crossing(jj = 1:8, d_ii = d4$d_ii) %>%
  ggplot(., aes(x = ordered(jj), y = d_ii - mean(d_ii))) +
    stat_summary(fun.data = "mean_cl_normal", shape = 16, size = 0.75) +
    geom_hline(yintercept = 0.25, linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(9/0.75, xlim = c(1, 8), ylim = c(-0.25, 0.50)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Condition",
      y = expression(italic(d)),
      title = "Complete Pooling"
    )
  
  export_png("figure-4d", width = 21.91/2)
  
  # Estimate multilevel model
  m0 <- lmer(d_ii ~ (1|jj), data = d4)
  
  # Pooling methods
  np <- d4 %>% 
    group_by(jj) %>% 
    summarise(mean = mean(d_ii), se = sd(d_ii)/sqrt(n())) %>%
    ungroup() %>%
    transmute(
      jj = jj,
      estimate  = mean,
      conf.low  = mean - 1.96 * se,
      conf.high = mean + 1.96 * se
    )
  cp <- d4 %>%
    summarise(mean = mean(d_ii), se = sd(d_ii)/sqrt(n())) %>%
    transmute(
      estimate  = mean,
      conf.low  = mean - 1.96 * se,
      conf.high = mean + 1.96 * se
    ) %>%
    crossing(jj = 1:8, .)
  pp <- arm::sim(m0, n.sims = 1000)@ranef$jj[,,1] %>%
    as_tibble() %>%
    gather("jj", "value") %>%
    mutate(jj = as.integer(jj)) %>%
    group_by(jj) %>%
    mutate(iter = 1:1000) %>%
    left_join(
      tibble(
        iter = 1:1000,
        intercept = arm::sim(m0, n.sims = 1000)@fixef[,1]
      ),
      by = "iter"
    ) %>%
    transmute(
      value = intercept + value
    ) %>%
    summarise(
      estimate  = median(value),
      conf.low  = quantile(value, 0.025),
      conf.high = quantile(value, 0.975)
    )
  
  # Figure 4e
  bind_rows(
    np %>% mutate(model = "No Pooling"),
    pp %>% mutate(model = "Partial Pooling"),
    cp %>% mutate(model = "Complete Pooling")
  ) %>%
    left_join(np %>% transmute(jj, reference = estimate, model = "Partial Pooling")) %>%
    mutate(
      model = ordered(
        model, levels = c("No Pooling", "Partial Pooling", "Complete Pooling")
      )
    ) %>% 
  ggplot(., aes(x = ordered(jj), y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), shape = 16, size = 0.5) +
    geom_point(aes(y = reference), shape = 21, size = 2) +
    geom_hline(yintercept = 0.25, linetype = "dashed", size = 1) +
    geom_hline(yintercept = 0, size = 1) +
    coord_fixed(9/0.75, xlim = c(1, 8), ylim = c(-0.25, 0.50)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(
      axis.line = element_blank(), 
      strip.background = element_blank(),
      strip.text = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))
    ) +
    labs(
      x = "Condition",
      y = expression(italic(d))
    ) + 
    facet_grid(. ~ model)
  
  export_png("figure-4e")
  

# Figure 5 ----------------------------------------------------------------

  set.seed(5034398)
  
  # Simulate data
  d5 <- mvrnorm(
    n = 30, 
    mu = c(4, 4),
    sigma = c(1, 0.65, 0.65, 1),
    names = c("x", "y", "ii")
  ) %>% lm(y ~ x, data = .) %>% augment()
  
  # LHS
  ggplot(d5, aes(x, y)) +
    geom_segment(
      aes(xend = x, y = .fitted, yend = .fitted + .resid),
      colour = "#FF005D",
      size = 0.5
    ) +
    geom_abline(
      intercept = 1.40,
      slope = 0.65,
      colour = "#1E88E5", 
      size = 1
    ) +
    geom_point(size = 2) +
    coord_fixed(1, xlim = c(1, 7), ylim = c(1, 7)) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:7) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      title = "Simple Linear Regression"
    )
  
  export_png("figure-5a", width = 21.91/2)
  
  # RHS
  ggplot(d5, aes(x = .resid)) +
    geom_histogram(
      aes(y = ..density..),
      binwidth = 1,
      colour = "white",
      fill = "#FF005D",
      size = 1
    ) +
    stat_function(
      fun = dnorm, 
      args = list(mean = 0, sd = sigma(lm(y ~ x, data = d5))),
      size = 1,
      xlim = c(-2.5, 2.5),
      n = 10001
    ) +
    coord_fixed(5/0.6, xlim = c(-2.5, 2.5), ylim = c(0.0, 0.6)) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(axis.line = element_blank()) +
    labs(
      x = "Residual Errors",
      y = "Density",
      title = ""
    )
  
  export_png("figure-5b", width = 21.91/2)
  

# Figure 6 ----------------------------------------------------------------
  
  set.seed(5034398)
  
  # Simulate data
  d6 <- tibble(ii = 1:100, jj = rep(1:4, 25)) %>%
  left_join(
    tibble(jj = 1:4, y_jj = rnorm(4, 1, 2)),
    by = "jj"
  ) %>%
  left_join(
    mvrnorm(
      n = 100, 
      mu = c(4, 4),
      sigma = c(1, 0.65, 0.65, 0.50),
      names = c("x", "y_ii", "ii")
    ),
    by = "ii"
  ) %>% 
  transmute(ii, jj, x, y = y_ii + y_jj)
  
  # Estimate multilevel model
  mlm <- lmer(y ~ 1 + x + (1|jj), data = d6)
  
  # Varying Intercepts
  ggplot(d6, aes(x, y)) +
    geom_point(aes(colour = factor(jj)), size = 2) +
    geom_abline(
      intercept = fixef(mlm)[1] + ranef(mlm)$jj$`(Intercept)`,
      slope = fixef(mlm)[2],
      colour = viridis::viridis(4),
      size = 1
    ) +
    coord_fixed(1, xlim = c(1, 7), ylim = c(1, 7)) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:7) +
    scale_colour_viridis_d() +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(legend.position = "none", axis.line = element_blank()) +
    labs(
      title = "Multilevel Model"
    )
  
  export_png("figure-6a", width = 21.91/2)
  
  # Varying Intercepts (2 groups)
  augment(mlm) %>%
    filter(jj %in% c(3, 4)) %>%
  ggplot(., aes(x, y)) +
    geom_segment(
      aes(xend = x, yend = .fitted),
      colour = "#FF005D",
      size = 0.5
    ) +
    annotate(
      geom = "segment",
      x = c(1, 7), 
      xend = c(1, 7), 
      y = c(fixef(mlm)[1] + fixef(mlm)[2], fixef(mlm)[1] + 7*fixef(mlm)[2]), 
      yend = c(fixef(mlm)[1] + fixef(mlm)[2] + ranef(mlm)$jj$`(Intercept)`[4],
               fixef(mlm)[1] + 7*fixef(mlm)[2] + ranef(mlm)$jj$`(Intercept)`[3]),
      colour = "#FFC107",
      size = 0.5
    ) +
    annotate(
      geom = "point",
      x = c(1, 7), 
      y = c(fixef(mlm)[1] + fixef(mlm)[2] + ranef(mlm)$jj$`(Intercept)`[4],
            fixef(mlm)[1] + 7*fixef(mlm)[2] + ranef(mlm)$jj$`(Intercept)`[3]),
      colour = "#FFC107",
      size = 2
    ) +
    geom_point(size = 2) +
    geom_abline(
      intercept = fixef(mlm)[1] + ranef(mlm)$jj$`(Intercept)`[3:4],
      slope = fixef(mlm)[2],
      colour = "#FFC107",
      size = 1
    ) +
    geom_abline(
      intercept = fixef(mlm)[1],
      slope = fixef(mlm)[2],
      colour = "#1E88E5",
      size = 1
    ) +
    coord_fixed(1, xlim = c(1, 7), ylim = c(1, 7)) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:7) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(legend.position = "none", axis.line = element_blank()) +
    labs(
      title = "Multilevel Model"
    )
  
  export_png("figure-6b", width = 21.91/2)
  

# Figure 7 ----------------------------------------------------------------
  
  set.seed(5034398)
  
  # Simulate data
  d7 <- bind_rows(
    mvrnorm(
      n = 25, 
      mu = c(4, 4.5),
      sigma = c(1, 0.65, 0.65, 0.50),
      names = c("x", "y", "ii")
    ) %>% mutate(jj = 1L, y = y - x*0.4),
    mvrnorm(
      n = 25, 
      mu = c(4, 3.5),
      sigma = c(1, 0.65, 0.65, 0.50),
      names = c("x", "y", "ii")
    ) %>% mutate(ii = ii + 25, jj = 2L, y = y + x*0.6)
  )
  
  # Varying Slopes
  d7 %>%
    mutate(.fitted = if_else(jj == 1L, 1.9 + 0.25 * x, 0.9 + 1.25 * x)) %>%
  ggplot(., aes(x, y)) +
    geom_segment(
      aes(xend = x, yend = .fitted),
      colour = "#FF005D",
      size = 0.5
    ) +
    geom_point(size = 2) +
    geom_abline(
      intercept = c(1.9, 0.9),
      slope = c(0.25, 1.25),
      colour = "#FFC107",
      size = 1
    ) +
    geom_abline(
      intercept = (1.9 + 0.9)/2,
      slope = (0.25 + 1.25)/2,
      colour = "#1E88E5",
      size = 1
    ) +
    coord_fixed(1, xlim = c(1, 7), ylim = c(1, 7)) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(breaks = 1:7) +
    cowplot::theme_cowplot(font_size = 16, line_size = 1) +
    cowplot::panel_border(colour = "black", size = 1) +
    theme(legend.position = "none", axis.line = element_blank()) +
    labs(
      title = "Varying (Random) Slopes"
    )
    
  export_png("figure-7b", width = 21.91/2)
  
  