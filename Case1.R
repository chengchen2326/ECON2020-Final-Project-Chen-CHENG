# --- Case 1: Both X1 and X2 highly confounded with U ---
library(multe)    # for contamination bias estimators
library(ggplot2)
library(tidyr)

set.seed(123)              # Reproducibility

# Simulation parameters
n <- 1000
U <- rnorm(n)                      # Latent variable
# High correlation for both treatments
alpha1 <- -0.5; alpha2 <- -0.5
rho1 <- 1; rho2 <- 1              # Strong effect of U on both treatments
p1 <- plogis(alpha1 + rho1 * U)   # P(X1=1|U)
p2 <- plogis(alpha2 + rho2 * U)   # P(X2=1|U)
X1 <- rbinom(n, 1, p1)
X2 <- rbinom(n, 1, p2)
# True parameters
beta1 <- 2; beta2 <- 3 # true effects for X1, X2
gamma <- 1  # confounder effect on Y
sigma <- 1
error <- rnorm(n, 0, sigma)
Y <- beta1*X1 + beta2*X2 + gamma*U + error

# Assemble and save dataset
if (!dir.exists("dta")) {
  dir.create("dta", recursive = TRUE)
  message("Created directory: ", normalizePath("dta"))
}

sim_data1 <- data.frame(Y=Y, X1=X1, X2=X2, U=U)
sim_data1$X1_f <- factor(sim_data1$X1, levels=c(0,1), labels=c("No","Yes"))
save(sim_data1, file="dta/sim_data_case1.rda")

# Fit OLS model for Case 1 and compute contamination-bias-corrected estimates
model1 <- lm(Y ~ X1_f + X2, data=sim_data1)
cb_res1 <- multe::multe(model1, "X1_f")
est_table1 <- export_est_binary(cb_res1)
print(est_table1)

#############################
# 3_tables_c1.R
#############################
source("functions.R")
# Load simulated data for Case 1
load("dta/sim_data_case1.rda")  # Data saved as sim_data1 in your 1_prepare_data.R for scenario 1

# Fit baseline OLS with X1 as a factor and X2 as control
model <- lm(Y ~ X1_f + X2, data = sim_data1)
cat("OLS Regression with Factor Treatment (Case 1):\n")
print(summary(model))

# Compute contamination bias estimates using multe
cb_results <- multe::multe(model, "X1_f")

# Produce comparison table using export_est()
comparison_table <- export_est_binary(cb_results)
cat("Comparison Table of Estimates (Case 1):\n")
print(comparison_table)

if (!dir.exists("output")) dir.create("output")
write.csv(comparison_table, file = "output/sim_comparison_table_case1.csv", row.names = FALSE)

#############################
# 4_figures_c1.R
#############################
library(ggplot2)
library(dplyr)
library(tidyr)
source("functions.R")

# Load the comparison table for Scenario 1
table_data <- read.csv("output/sim_comparison_table_case1.csv", stringsAsFactors = FALSE)

# Reshape to long format for plotting
comparison_long <- table_data %>%
  pivot_longer(cols = c("reg_est", "own_est", "ate_est", "ew_est", "cw_est"),
               names_to = "estimator", values_to = "estimate")
comparison_long$true <- 2  # True effect for X1

est_plot <- ggplot(comparison_long, aes(x = estimator, y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(aes(yintercept = true), linetype = "dashed", color = "red") +
  labs(title = "Case 1: Treatment Effect Estimates for X1",
       subtitle = "Both X1 and X2 are highly confounded with U (true effect = 2)",
       x = "Estimator", y = "Estimated Effect") +
  theme_minimal()

if (!dir.exists("output")) dir.create("output")
ggsave("output/sim_estimates_plot_case1.png", est_plot, width = 6, height = 4, units = "in")
ggsave("output/sim_estimates_plot_case1.pdf", est_plot, width = 6, height = 4, units = "in")
