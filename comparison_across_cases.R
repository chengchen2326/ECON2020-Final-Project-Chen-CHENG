#############################
# comparison_across_scenarios.R
#############################
library(dplyr)
library(tidyr)
library(ggplot2)

# Define scenario labels and corresponding file names
scenario_files <- list(
  S1 = "output/sim_comparison_table_case1.csv",
  S2 = "output/sim_comparison_table_case2.csv",
  S3 = "output/sim_comparison_table_case3.csv",
  S4 = "output/sim_comparison_table_case4.csv"
)

# Read each CSV file and add a 'scenario' column
list_tables <- lapply(names(scenario_files), function(scen) {
  df <- read.csv(scenario_files[[scen]], stringsAsFactors = FALSE)
  df$scenario <- scen
  return(df)
})

# Combine all scenario tables into one data frame
combined_table <- bind_rows(list_tables)

# Save the combined table as CSV
if (!dir.exists("output")) dir.create("output")
write.csv(combined_table, file = "output/combined_comparison_table.csv", row.names = FALSE)

# Reshape the combined table to long format for plotting.
# We assume that each CSV has columns: reg_est, own_est, ate_est, ew_est, cw_est.
combined_long <- combined_table %>%
  pivot_longer(
    cols = c("reg_est", "own_est", "ate_est", "ew_est", "cw_est"),
    names_to = "estimator",
    values_to = "estimate"
  ) %>%
  mutate(estimate = as.numeric(estimate))

# Create a grouped bar plot comparing estimates across scenarios.
combined_plot <- ggplot(combined_long, aes(x = scenario, y = estimate, fill = estimator)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  labs(title = "Comparison of Treatment Effect Estimates Across Scenarios",
       subtitle = "True effect = 2 (dashed red line)",
       x = "Scenario", y = "Estimated Effect") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Save the plot as both PNG and PDF
ggsave("output/combined_estimates_plot.png", combined_plot, width = 8, height = 5, units = "in")
ggsave("output/combined_estimates_plot.pdf", combined_plot, width = 8, height = 5, units = "in")


#############################
# create_summary_table_pdf.R
#############################
# Load required packages
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("grid")) install.packages("grid")
library(gridExtra)
library(grid)

# Create the summary table data frame
table_data <- data.frame(
  Scenario = c("Both X1 & X2 high corr with U",
               "X1 high corr, X2 low corr",
               "X1+U, X2−U (opposite corr)",
               "X2 deterministic f(U)"),
  `OLS (PL)` = c("~2.70", "~2.65", "~2.75", "~2.20"),
  OWN        = c("~2.05", "~2.60", "~2.10", "~1.95"),
  ATE        = c("~2.55", "~2.60", "~2.50", "~2.10"),
  EW         = c("2.50", "2.65", "2.40", "2.00"),
  CW         = c("2.10", "2.62", "2.05", "1.98"),
  stringsAsFactors = FALSE
)

# Convert the data frame into a table grob.
# The argument rows = NULL removes row names.
table_grob <- tableGrob(table_data, rows = NULL)

# Ensure output directory exists
if (!dir.exists("output")) dir.create("output")

# Open a PDF device and draw the table grob
pdf("output/summary_table.pdf", width = 8, height = 4)
grid.draw(table_grob)
dev.off()

cat("Summary table has been printed to 'output/summary_table.pdf'\n")
