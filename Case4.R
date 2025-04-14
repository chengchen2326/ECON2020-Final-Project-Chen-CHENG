# --- Case 4: X2 from a nonlinear/deterministic function of U ---
set.seed(123)
n <- 1000
U <- rnorm(n)
# X1 via logistic on U, X2 deterministic nonlinear of U (e.g., threshold)
alpha1 <- -0.5; rho1 <- 1
p1 <- plogis(alpha1 + rho1 * U)    # X1 probability depends on U
X1 <- rbinom(n, 1, p1)
X2 <- ifelse(U > 0, 1, 0)          # Deterministic: X2 = 1 if U positive, else 0
beta1 <- 2; beta2 <- 3; gamma <- 1; sigma <- 1
error <- rnorm(n, 0, sigma)
Y <- beta1*X1 + beta2*X2 + gamma*U + error

# Assemble and save dataset
if (!dir.exists("dta")) {
  dir.create("dta", recursive = TRUE)
  message("Created directory: ", normalizePath("dta"))
}

sim_data4 <- data.frame(Y=Y, X1=X1, X2=X2, U=U)
sim_data4$X1_f <- factor(sim_data4$X1, levels=c(0,1), labels=c("No","Yes"))
save(sim_data4, file="dta/sim_data_case4.rda")

model4 <- lm(Y ~ X1_f + X2, data=sim_data4)
cb_res4 <- multe::multe(model4, "X1_f")
est_table4 <- export_est_binary(cb_res4)
print(est_table4)

#############################
# 3_tables_v4.R
#############################
source("functions.R")
# Load simulated data for Scenario 4
load("dta/sim_data_case4.rda")  # Data saved as sim_data4 in your 1_prepare_data.R for scenario 4

# Fit baseline OLS with X1 as a factor and X2 as control
model <- lm(Y ~ X1_f + X2, data = sim_data4)
cat("OLS Regression with Factor Treatment (Case 4):\n")
print(summary(model))

# Compute contamination bias estimates using multe
cb_results <- multe::multe(model, "X1_f")

# Produce comparison table using export_est()
comparison_table <- export_est_binary(cb_results)
cat("Comparison Table of Estimates (Case 4):\n")
print(comparison_table)

if (!dir.exists("output")) dir.create("output")
write.csv(comparison_table, file = "output/sim_comparison_table_case4.csv", row.names = FALSE)

#############################
# 4_figures_v4.R
#############################
library(ggplot2)
library(dplyr)
library(tidyr)
source("functions.R")

# Load the comparison table for Scenario 4
table_data <- read.csv("output/sim_comparison_table_case4.csv", stringsAsFactors = FALSE)

comparison_long <- table_data %>%
  pivot_longer(cols = c("reg_est", "own_est", "ate_est", "ew_est", "cw_est"),
               names_to = "estimator", values_to = "estimate")
comparison_long$true <- 2

est_plot <- ggplot(comparison_long, aes(x = estimator, y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(aes(yintercept = true), linetype = "dashed", color = "red") +
  labs(title = "Case 4: Treatment Effect Estimates for X1",
       subtitle = "X2 is a deterministic function of U; controls U well",
       x = "Estimator", y = "Estimated Effect") +
  theme_minimal()

if (!dir.exists("output")) dir.create("output")
ggsave("output/sim_estimates_plot_case4.png", est_plot, width = 6, height = 4, units = "in")
ggsave("output/sim_estimates_plot_case4.pdf", est_plot, width = 6, height = 4, units = "in")

