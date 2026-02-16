# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set working directory (replace with your directory)
setwd("C:/users/tangl/Desktop/results")

# List of file names or paths, assuming all design files are stored in the working directory
file_list <- list.files(pattern = "*.csv") # This will read all CSV files in the folder
file_list
# Alternatively, if you have a specific list of file paths:
# file_list <- c("results 1design_grid.csv", "results 2.reduced_grid_design.csv", "results 3.random_design_1.csv", "expert_design_1.csv", "expert_design_2.csv", "user_design_more_efficient.csv", "user_design_less_efficient.csv")

# Create an empty list to store the data
data_list <- list()

# Loop over the file list and read each file
for (file in file_list) {
  # Read the CSV file and store in the list
  data <- read.csv(file)
  # Extract design name from file name (you can adjust the logic to extract a cleaner name)
  design_name <- gsub(".csv", "", file)
  # Add a new column 'design' to store the name of the design
  data$design <- design_name
  # Append the data to the list
  data_list[[design_name]] <- data
}

str(data_list)
# Combine all data into one data frame
combined_data <- bind_rows(data_list, .id = "design")
str(combined_data)

# Convert 'design' to factor for better plotting
combined_data$design <- factor(combined_data$design)

# Summary statistics for each design
summary_stats <- aggregate(combined_data$time ~ combined_data$design, FUN = function(x) c(mean = mean(x), sd = sd(x)))
print(summary_stats)


#1./ ploting
# Compare detection times across designs
ggplot(combined_data, aes(x = design, y = time)) +
  geom_boxplot() +
  labs(title = "Comparison of Detection Times by Trap Design 1-9", x = "Design", y = "Time to Detection (days)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
 # theme_classic() +  # White background
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plot
    panel.background = element_rect(fill = "white"),  # Ensure background is white
    plot.background = element_rect(fill = "white", color = "black", size = 1)  # Border around the entire plot area
  ) +
  theme(legend.position = "right")

# Histogram of time to detection for each design
ggplot(combined_data, aes(x = time, fill = design)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Time to First Detection by Trap Design 1-9", x = "Time to Detection (days)", y = "Frequency") +
  theme_minimal()+
  theme_classic() +  # White background
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plot
    panel.background = element_rect(fill = "white"),  # Ensure background is white
    plot.background = element_rect(fill = "white", color = "black", size = 1)  # Border around the entire plot area
  ) +
  theme(legend.position = "right")


# Histogram of time to detection for each design, faceted by design
ggplot(combined_data, aes(x = time)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Time to First Detection by Trap Design 1-9", x = "Time to Detection (days)", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ design)+  # Facet by design
theme_classic() +  # White background
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plot
    panel.background = element_rect(fill = "white"),  # Ensure background is white
    plot.background = element_rect(fill = "white", color = "black", size = 1)  # Border around the entire plot area
  ) +
  theme(legend.position = "right")

# Load necessary libraries
# Assuming 'combined_data' contains the time to detection for each design
# Add a column for CDF (cumulative distribution function)
combined_data <- combined_data %>%
  group_by(design) %>%
  arrange(time) %>%
  mutate(cdf = cumsum(!is.na(time)) / n())  # Calculate CDF for each design

# CDF plots faceted by design
ggplot(combined_data, aes(x = time, y = cdf, color = design)) +
  geom_line(size = 1) +
  labs(title = "Cumulative Distribution of Time to First Detection by Trap Design 1-9",
       x = "Time to Detection (days)",
       y = "CDF") +
  theme_minimal() +
  theme(legend.position = "right") +
  #facet_wrap(~ design, scales = "free")  # Create facets for each design
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a black border around the plot
    panel.background = element_rect(fill = "white"),  # Ensure background is white
    plot.background = element_rect(fill = "white", color = "black", size = 1)  # Border around the entire plot area
  ) +
  theme(legend.position = "right")

#2./90th and 99th percentile 
# Calculate 90th and 99th percentile detection times
percentile_90 <- aggregate(time ~ design, data = combined_data, function(x) quantile(x, 0.90))
percentile_99 <- aggregate(time ~ design, data = combined_data, function(x) quantile(x, 0.99))

print(percentile_90)
print(percentile_99)

# Save results
write.csv(summary_stats, "summary_statistics.csv")
write.csv(percentile_90, "percentile_90.csv")
write.csv(percentile_99, "percentile_99.csv")



#3./ pairwise comparison
# Load necessary libraries
library(dplyr)

# Assuming combined_data contains the detection times for each design
# and the column "time" contains the detection times and "design" contains the design labels.

# Perform pairwise t-tests
pairwise_results <- pairwise.t.test(combined_data$time, combined_data$design, p.adjust.method = "bonferroni")

# Print the results
print(pairwise_results)

# If you want to save the results to a file
write.csv(as.data.frame(pairwise_results$p.value), "pairwise_t_test_results.csv")



