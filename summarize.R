# Load required libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(magrittr)
library(dplyr)
library(xtable)
library(gridExtra)
library(reshape2)

# Function to extract parameters from the filename
extract_settings <- function(filename) {
  basename <- basename(filename)
  # Extract parameters n, U, V, and task_id from the filename
  pattern <- "results_n(\\d+)_U([-0-9\\.]+)_V([-0-9\\.]+)_task(\\d+)\\.rds"
  matches <- regexec(pattern, basename)
  params <- regmatches(basename, matches)[[1]][-1]
  params <- as.numeric(params)
  names(params) <- c("n", "U", "V", "task_id")
  return(params)
}

# Read all result files
files <- list.files("results", pattern = "results_n.*\\.rds", full.names = TRUE)
nsim <- length(files)

# Initialize a data frame to store results
df <- data.frame(
  n = numeric(nsim),
  U = numeric(nsim),
  V = numeric(nsim),
  task_id = integer(nsim),
  est = numeric(nsim),
  se = numeric(nsim),
  lower = numeric(nsim),
  upper = numeric(nsim),
  true_value = numeric(nsim),
  stringsAsFactors = FALSE
)

# Loop over files and extract data
for (i in 1:nsim) {
  # Extract settings from the filename
  settings <- extract_settings(files[i])
  n <- settings["n"]
  U <- settings["U"]
  V <- settings["V"]
  task_id <- settings["task_id"]
  
  # Read the results
  fit <- readRDS(files[i])
  
  # Extract estimate and standard error
  est <- fit$theta
  se <- fit$var
  
  # Compute 95% confidence interval
  lower <- est - qnorm(.975) * se
  upper <- est + qnorm(.975) * se
  
  # Compute true value
  true_value <- (U - 1.25 + V / 2) / 2
  
  # Store in data frame
  df$n[i] <- n
  df$U[i] <- U
  df$V[i] <- V
  df$task_id[i] <- task_id
  df$est[i] <- est
  df$se[i] <- se
  df$lower[i] <- lower
  df$upper[i] <- upper
  df$true_value[i] <- true_value
}

# Compute errors and coverage indicators
df$error <- df$est - df$true_value
df$sq_error <- df$error^2
df$coverage <- as.numeric(df$true_value >= df$lower & df$true_value <= df$upper)

# Summarize results by scenario
summary <- df %>%
  group_by(n, U, V) %>%
  summarize(
    true_value = first(true_value),
    ME = mean(error),
    MSE = mean(sq_error),
    nMSE = first(n) * mean(sq_error),
    Coverage = mean(coverage),
    .groups = 'drop'  # Ensure the data is ungrouped after summarizing
  )

# Create a grouping variable for plotting (same U and V)
summary$UV_Scenario <- factor(paste("U=", summary$U, ", V=", summary$V, sep=""))

# Order the summary by U, V, and sample size n
summary <- summary %>%
  arrange(U, V, n)

# Define a custom plotting theme
my_theme <- theme_bw(base_size = 14) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 18),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

# Generate n*MSE plot with lines connecting points of the same U and V
nMSE_plot <- ggplot(summary, aes(x = n, y = nMSE, color = UV_Scenario, group = UV_Scenario)) +
  geom_point(size = 4) +
  geom_line(size = 1.2) +
  labs(
    title = "n*MSE vs. Sample Size",
    x = "Sample Size (n)",
    y = expression(n * MSE),
    color = "Scenario"
  ) +
  scale_x_continuous(breaks = c(1000, 5000)) +
  my_theme

# Generate Coverage Probability plot with lines connecting points of the same U and V
Coverage_plot <- ggplot(summary, aes(x = n, y = Coverage, color = UV_Scenario, group = UV_Scenario)) +
  geom_point(size = 4) +
  geom_line(size = 1.2) +
  labs(
    title = "Coverage Probability vs. Sample Size",
    x = "Sample Size (n)",
    y = "Coverage Probability",
    color = "Scenario"
  ) +
  scale_x_continuous(breaks = c(1000, 5000)) +
  my_theme

# Save the plots
ggsave("nMSE_plot.eps", plot = nMSE_plot, width = 12, height = 8, device = "eps")
ggsave("Coverage_plot.eps", plot = Coverage_plot, width = 12, height = 8, device = "eps")

# Create a LaTeX table including nMSE and Coverage
scenario_table <- summary %>%
  select(n, U, V, true_value, nMSE, Coverage)

# Format and round the values
scenario_table <- scenario_table %>%
  mutate(
    n = sprintf("$%d\\times 10^3$", n / 1000),
    U = as.integer(U),
    V = as.integer(V),
    true_value = round(true_value, 3),
    nMSE = round(nMSE, 3),
    Coverage = round(Coverage, 3)
  )

# Set column names
colnames(scenario_table) <- c("$n$", "$U$", "$V$", "$\\theta$", "$n \\cdot \\text{MSE}$", "Coverage")

# Generate LaTeX code for the table
latex_scenario_table <- xtable(
  scenario_table,
  caption = "Scenarios with Corresponding Parameter Values, $n \\cdot \\text{MSE}$, and Coverage Probability",
  label = "tab:scenarios",
  align = c("l", "c", "c", "c", "c", "c", "c")
)

print(
  latex_scenario_table,
  file = "scenario_table.tex",
  include.rownames = FALSE,
  include.colnames = TRUE,
  floating = FALSE,
  sanitize.text.function = identity,
  hline.after = c(-1, 0, nrow(scenario_table))
)

# Save the summary data for future reference
saveRDS(summary, "summary.rds")
