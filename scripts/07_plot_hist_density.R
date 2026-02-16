#code for histograms of the results, or probability density estimates, or cumulative probability curves
# Load the necessary libraries
library(ggplot2)

# Load the dataset
str(allres)

# Create Histogram for x
ggplot(allres, aes(x = time)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of time", x = "time", y = "Frequency")

# Create Probability Density Estimates for x 
ggplot(allres, aes(x = time)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Probability Density Estimate of time", x = "time", y = "Density")

# Create Cumulative Probability Curves for x 
ggplot(allres, aes(x = time)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(title = "Cumulative Probability Curve of time", x = "time", y = "Cumulative Probability")

