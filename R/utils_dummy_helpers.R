#' dummy_helpers to simulate some fake flight-height simulations.
#'
#' @description A utils function
#'
#' @param seed An integer seed for reproducibility.
#' @param max_height The maximum height for the distributions.
#' @param n The number of unique distributions to simulate.
#'
#' @return A data frame containing flight-height distributions.
#'
#' @noRd
dummy_fheight_dists <- function(max_height = 100, seed = 123, n = 3) {
  
  # Create a grid of heights
  height_steps <- seq(0, max_height, length.out = 100)
  t <- seq(0, 1, length.out = length(height_steps))
  
  dists <- replicate(n, {
    seed <- seed + 1
    # Number of kernels to sum
    num_kernels <- sample(3:6, 1)
    
    # Random centers and widths for the kernels
    centers <- runif(num_kernels, 0, 1)
    widths <- runif(num_kernels, 0.05, 0.3)
    amplitudes <- runif(num_kernels, 0.5, 1.5)
    
    # Calculate kernel values
    # We use the Gaussian function: amp * exp(-(t - center)^2 / (2 * width^2))
    kernel_vals <- sapply(t, function(x) {
      sum(amplitudes * exp(-(x - centers)^2 / (2 * widths^2)))
    })
    
    # Normalize so probabilities sum to 1
    kernel_vals / sum(kernel_vals)
  }, simplify = FALSE)
  
  # Convert to long format dataframe
  df <- data.frame(
    f_id = rep(seq_len(n), each = length(height_steps)),
    height = rep(height_steps, n),
    prob = unlist(dists)
  )
  
  return(df)
}

dummy_fheight_plot <- function(df, risk_min = 50, risk_max = 70) {
  # Create a plotly plot of the distributions
  p1 <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = df,
      x = ~height,
      y = ~prob,
      color = ~as.factor(f_id),
      type = 'scatter',
      mode = 'lines',
      line = list(width = 2)
    ) %>%
    plotly::layout(title = "Dummy Flight Height Distributions",
                   xaxis = list(title = "Height"),
                   yaxis = list(title = "Probability"))
  
  # If risk_min and risk_max are not NA, add a rectangular bar between them
  if (!is.na(risk_min) && !is.na(risk_max)) {
    p1 <- p1 %>%
      plotly::add_trace(
        x = c(risk_min, risk_max, risk_max, risk_min, risk_min),
        y = c(0, 0, max(df$prob), max(df$prob), 0),
        type = 'scatter',
        mode = 'lines',
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.2)',
        line = list(color = 'rgba(255, 0, 0, 0)'),
        name = 'Risk Zone'
      )
  }

  p1
}
dummy_fheight_plot(test)
