# --- Case 2: Only X1 highly correlated with U, X2 weakly/not correlated ---
set.seed(123)
n <- 1000
U <- rnorm(n)
# X1 strongly dependent on U, X2 nearly independent of U
alpha1 <- -0.5; alpha2 <- -0.5
rho1 <- 1; rho2 <- 0              # X1 correlated with U, X2 not correlated
p1 <- plogis(alpha1 + rho1 * U)   # P(X1=1|U) depends on U
p2 <- plogis(alpha2 + rho2 * U)   # P(X2=1|U) ~ constant (rho2=0)
X1 <- rbinom(n, 1, p1)
X2 <- rbinom(n, 1, p2)            # effectively Bernoulli with ~constant probability
# True parameters and outcome
beta1 <- 2; beta2 <- 3; gamma <- 1; sigma <- 1
error <- rnorm(n, 0, sigma)
Y <- beta1*X1 + beta2*X2 + gamma*U + error

# Assemble and save dataset
if (!dir.exists("dta")) {
  dir.create("dta", recursive = TRUE)
  message("Created directory: ", normalizePath("dta"))
}

sim_data2 <- data.frame(Y=Y, X1=X1, X2=X2, U=U)
sim_data2$X1_f <- factor(sim_data2$X1, levels=c(0,1), labels=c("No","Yes"))
save(sim_data2, file="dta/sim_data_case2.rda")

model2 <- lm(Y ~ X1_f + X2, data=sim_data2)
cb_res2 <- multe::multe(model2, "X1_f")
est_table2 <- export_est_binary(cb_res2)
print(est_table2)

#############################
# 3_tables_c2.R
#############################
source("functions.R")
# Load simulated data for Scenario 2
load("dta/sim_data_case2.rda")  # Data saved as sim_data2 in your 1_prepare_data.R for scenario 2

# Fit baseline OLS with X1 as a factor and X2 as control
model <- lm(Y ~ X1_f + X2, data = sim_data2)
cat("OLS Regression with Factor Treatment (Case 2):\n")
print(summary(model))

# Compute contamination bias estimates using multe
cb_results <- multe::multe(model, "X1_f")

# Produce comparison table using export_est()
comparison_table <- export_est_binary(cb_results)
cat("Comparison Table of Estimates (Case 2):\n")
print(comparison_table)

if (!dir.exists("output")) dir.create("output")
write.csv(comparison_table, file = "output/sim_comparison_table_case2.csv", row.names = FALSE)

#############################
# 4_figures_c2.R
#############################
library(ggplot2)
library(dplyr)
library(tidyr)
source("functions.R")

# Load the comparison table for Scenario 2
table_data <- read.csv("output/sim_comparison_table_case2.csv", stringsAsFactors = FALSE)

comparison_long <- table_data %>%
  pivot_longer(cols = c("reg_est", "own_est", "ate_est", "ew_est", "cw_est"),
               names_to = "estimator", values_to = "estimate")
comparison_long$true <- 2

est_plot <- ggplot(comparison_long, aes(x = estimator, y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(aes(yintercept = true), linetype = "dashed", color = "red") +
  labs(title = "Case 2: Treatment Effect Estimates for X1",
       subtitle = "X1 is highly correlated with U; X2 is nearly independent of U",
       x = "Estimator", y = "Estimated Effect") +
  theme_minimal()

if (!dir.exists("output")) dir.create("output")
ggsave("output/sim_estimates_plot_case2.png", est_plot, width = 6, height = 4, units = "in")
ggsave("output/sim_estimates_plot_case2.pdf", est_plot, width = 6, height = 4, units = "in")

