# Load necessary libraries
library(ggplot2)
library(dplyr)
library(boot)  # For bootstrapping

# Function to calculate CDF for a given sample
cdf_function <- function(data, times) {
  ecdf_func <- ecdf(data)
  return(ecdf_func(times))
}

# Bootstrapping function to generate multiple samples and CDFs
bootstrap_cdf <- function(data, n_bootstrap = 1000) {
  times <- sort(unique(data$time))  # Unique time points
  boot_cdfs <- matrix(NA, nrow = n_bootstrap, ncol = length(times))
  
  # Bootstrapping loop
  for (i in 1:n_bootstrap) {
    sample_data <- data %>% sample_frac(replace = TRUE)  # Resample with replacement
    boot_cdfs[i, ] <- cdf_function(sample_data$time, times)
  }
  
  # Calculate confidence intervals (2.5% and 97.5%)
  ci_lower <- apply(boot_cdfs, 2, quantile, probs = 0.025)
  ci_upper <- apply(boot_cdfs, 2, quantile, probs = 0.975)
  
  return(data.frame(time = times, ci_lower = ci_lower, ci_upper = ci_upper))
}

# Apply the bootstrapping CDF calculation for each design
bootstrap_results <- combined_data %>%
  group_by(design) %>%
  do(bootstrap_cdf(.))  # Apply the bootstrap_cdf function

# Merge the bootstrap confidence intervals back with the original data
combined_data <- combined_data %>%
  left_join(bootstrap_results, by = c("design", "time"))

# Plot CDF with confidence intervals
ggplot(combined_data, aes(x = time, y = cdf, color = design)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = design), alpha = 0.2) +  # Confidence intervals
  labs(title = "Cumulative Distribution of Time to First Detection by Design with Confidence Intervals",
       x = "Time to Detection (days)", y = "CDF") +
  theme_minimal() +
  theme(legend.position = "right")
