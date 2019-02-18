  # Function for standardising variables
  standardise <- function(v) ( v - mean(v, na.rm = TRUE) ) / sd(v, na.rm = TRUE)
  
  # Function for plotting varying (random) intercepts
  plot_intercepts <- function(m) {
    arm::sim(m, n.sims = 1000)@ranef$jj[,,1] %>%
      as_tibble() %>%
      gather("jj", "value") %>%
      mutate(jj = as.integer(jj)) %>%
      group_by(jj) %>%
      summarise(
        estimate  = median(value),
        conf.low  = quantile(value, 0.025),
        conf.high = quantile(value, 0.975)
      ) %>%
    ggplot(., aes(x = ordered(jj, levels = rev(unique(jj))), y = estimate)) +
      geom_point() +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey20") +
      scale_y_continuous(breaks = seq(-3, 3, 1)) +
      coord_flip() +
      theme(axis.ticks.y = element_blank()) +
      labs(
        x = "Neighbourhood",
        y = "Estimate",
        title = "Varying (Random) Intercepts",
        subtitle = "Parameter Estimates with 95% Confidence Intervals"
      )
  }
  
  # Function for plotting varying (random) slopes
  plot_slopes <- function(m) {
    # Predict
    pred <- crossing(
      jj = 1:length(ranef(m)$jj$`(Intercept)`),
      x  = -5:5
    ) %>%
    left_join(
      tibble(
        intercept = ranef(m)$jj$`(Intercept)`,
        slope = ranef(m)$jj$contact,
        jj = 1:length(slope)
      ),
      by = "jj"
    ) %>%
    mutate(
      y = intercept + x * slope
    )
    
    # Visualise
    ggplot(pred, aes(x = x, y = y)) +
      geom_line(aes(group = jj)) +
      geom_vline(xintercept = 0, colour = "grey20") +
      geom_point(data = filter(pred, x == 0)) +
      scale_x_continuous(breaks = -5:5) +
      scale_y_continuous(breaks = -5:5) +
      coord_fixed(1, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
      labs(
        x = "Contact",
        y = "Attitudes",
        title = "Varying (Random) Slopes",
        subtitle = "Estimated Slopes for Standardised Variables"
      ) %>% return()
  }
