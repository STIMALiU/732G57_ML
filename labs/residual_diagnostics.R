library(ggplot2)
library(cowplot)

#-------------------------------------------------------------------------------
# residual_diagnostics()
#-------------------------------------------------------------------------------
# res_vect : vector with residuals
# fit_vect : vector with fitted values
# alpha    : transparency for points
# binwidth : histogram binwidth
#-------------------------------------------------------------------------------

residual_diagnostics <- function(
    res_vect,
    fit_vect,
    alpha = 0.3,
    binwidth = NULL
) {
  
  # Remove observations with missing values
  keep <- complete.cases(res_vect, fit_vect)
  
  res_vect <- res_vect[keep]
  fit_vect <- fit_vect[keep]
  
  #---------------------------------------------------------------------------
  # Histogram of residuals
  #---------------------------------------------------------------------------
  
  res_df <- data.frame(residuals = res_vect)
  
  p1 <- ggplot(res_df, aes(x = residuals)) +
    geom_histogram(
      aes(y = after_stat(density)),
      binwidth = binwidth,
      alpha = alpha
    ) +
    stat_function(
      fun = dnorm,
      args = list(
        mean = mean(res_df$residuals),
        sd = sd(res_df$residuals)
      ),
      linewidth = 1,
      col = "blue"
    ) +
    xlab("Residuals") +
    ylab("Density")
  
  #---------------------------------------------------------------------------
  # Fitted values vs residuals
  #---------------------------------------------------------------------------
  
  df_fit <- data.frame(
    fitted_values = fit_vect,
    residuals = res_vect
  )
  
  p2 <- ggplot(
    df_fit,
    aes(x = fitted_values, y = residuals)
  ) +
    geom_point(alpha = alpha) +
    geom_hline(
      yintercept = 0,
      col = "blue"
    ) +
    xlab("Fitted values") +
    ylab("Residuals")
  
  #---------------------------------------------------------------------------
  # Observation order vs residuals
  #---------------------------------------------------------------------------
  
  df_index <- data.frame(
    index = seq_along(res_vect),
    residuals = res_vect
  )
  
  p3 <- ggplot(
    df_index,
    aes(x = index, y = residuals)
  ) +
    geom_point(alpha = alpha) +
    geom_hline(
      yintercept = 0,
      col = "blue"
    ) +
    xlab("Observation index") +
    ylab("Residuals")
  
  #---------------------------------------------------------------------------
  # Normal Q-Q plot (original residual scale)
  #---------------------------------------------------------------------------
  
  df_qq <- data.frame(
    residuals = res_vect
  )
  
  p4 <- ggplot(
    df_qq,
    aes(sample = residuals)
  ) +
    stat_qq(
      alpha = alpha,
      col = "blue"
    ) +
    stat_qq_line() +
    ylab("Residuals")
  
  #---------------------------------------------------------------------------
  # Combine plots
  #---------------------------------------------------------------------------
  
  plot_final <- plot_grid(
    p4, p2,
    p1, p3,
    nrow = 2
  )
  
  return(plot_final)
}