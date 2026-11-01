# Recompute observed-detection summaries and trap-effort comparisons.
# Does NOT calculate detection probability: non-detection runs and total attempts
# are not available in the archived outcome CSVs.
base <- "results"
files <- c(
  "Grid"="results 1.design_grid.csv",
  "Reduced grid"="results 2.reduced_grid_design.csv",
  "Random design 1"="results 3.random_design_1.csv",
  "Random design 2"="results 4.random_design_2.csv",
  "Random design 3"="results 5.random_design_3.csv",
  "Expert design 1"="results 6.expert design 1.csv",
  "Expert design 2"="results 7.expert design 2.csv",
  "Clustered design"="results 8.Clustered Design.csv",
  "Sparse-border design"="results 9.sparse_border_design.csv"
)
designs <- c(
  "data/1design_grid.csv", "data/2.reduced_grid_design.csv",
  "data/3.random_design_1.csv", "data/4.random_design_2.csv",
  "data/5.random_design_3.csv", "data/6.expert design 1.csv",
  "data/7.expert design 2.csv", "data/8.Clustered Design.csv",
  "data/9.sparse_border_design.csv"
)
stopifnot(all(file.exists(file.path(base, unname(files)))), all(file.exists(designs)))
summary <- do.call(rbind, lapply(seq_along(files), function(i) {
  d <- read.csv(file.path(base, files[[i]]), check.names=FALSE)
  trap <- read.csv(designs[i], check.names=FALSE)
  stopifnot("time" %in% names(d), all(c("x", "y") %in% names(trap)))
  x <- d$time
  stopifnot(is.numeric(x), all(is.finite(x)), all(x >= 2 & x <= 400))
  data.frame(design=names(files)[i], traps=nrow(trap),
             recorded_detections=length(x),
             mean_detection_day=mean(x), sd_detection_day=sd(x),
             median_detection_day=median(x),
             p90_detection_day=unname(quantile(x,0.90)),
             p99_detection_day=unname(quantile(x,0.99)),
             earliest_detection_day=min(x), latest_detection_day=max(x),
             stringsAsFactors=FALSE)
}))
baseline <- summary$mean_detection_day[summary$design=="Grid"]
summary$days_vs_grid <- summary$mean_detection_day-baseline
summary$trap_fraction_vs_grid <- summary$traps/summary$traps[summary$design=="Grid"]
summary$detection_rate <- NA_real_ # unknown denominator, do not infer
summary <- summary[order(summary$mean_detection_day), ]
write.csv(summary, file.path(base,"trap_effort_detection_comparison.csv"), row.names=FALSE)
print(summary, row.names=FALSE)
cat("\nImportant: means and percentiles are conditional on saved detected runs.\n")
cat("Archived CSVs do not identify attempts that failed to detect by day 400.\n")
