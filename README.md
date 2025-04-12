# ECON2020-Final-Project-Chen-CHENG
Contamination Bias Simulation: Two-Treatment Simulation with Multiple Data-Generating Processes
# Data and Code for: Contamination Bias Simulation Project
# Author: Chen Cheng

## Overview

This project uses simulation to study contamination bias in linear regressions as described in *Contamination Bias in Linear Regressions* by Goldsmith-Pinkham, Hull, and Kolesár (2024). We simulate data with two treatments (X1 and X2) influenced by an unobserved confounder U. Four scenarios with different correlation structures between U and the treatments are considered:

1. **Both Highly Confounded:** Both X1 and X2 are strongly correlated with U.
2. **One Strong, One Weak:** X1 is strongly confounded while X2 is nearly independent of U.
3. **Opposite Correlation:** X1 is positively correlated and X2 is negatively correlated with U.
4. **Deterministic X2:** X2 is generated as a deterministic function of U.

For each scenario, OLS estimates and three contamination bias correction estimators (OWN, EW, CW) are computed using the `multe` package. The outputs are saved as PDF tables (with simulation results and density plots) in the `output/` directory.

## Project Structure
ContaminationBiasSim/ 
-  0_setup.R # Package installation and environment setup
-  functions.R # Helper functions (export_est, etc.)
-  dta/ # Simulated data files
-  output/ # PDF tables and figures
-  renv/ # Managed package environment (created by renv)
-  renv.lock # Lockfile for package versions
-  case1.R  # R scripts for each scenario and comparisons
-  case2.R  
-  case3.R  
-  case4.R  
-  comparison_across_scenarios.R 
-  main.R # Master script that runs all analyses 
-  README.md # Project documentation

## Requirements

- **R 4.3.1 or higher**
- **renv** – used for package management (activate by running `renv::restore()`)
- The following R packages (versions recorded in `renv.lock`): dplyr, ggplot2, multe, tidyr, readstata13, remotes, testthat, gridExtra, and grid.

## Instructions

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/<chengchen2326>/ECON2020-Final-Project-Chen-CHENG.git
   cd ECON2020-Final-Project-Chen-CHENG
   
2. **Restore the R Environment:**

   In R (or RStudio), run:
   ```r
   install.packages("renv")
   renv::restore()

3. **Run the Analysis:**
   Execute the master script:
   ```r
   source("main.R")
   ```

This will run the simulations for all four cases, generate case-specific PDF tables and density plots in the output/ folder, and compile a summary table and summary figure (both in PDF).

4. **Viewing Outputs:**

- Case-specific PDF tables: output/sim_comparison_table_case1.pdf, output/sim_comparison_table_case2.pdf, output/sim_comparison_table_case3.pdf, output/sim_comparison_table_case4.pdf

- Case-specific density plots: output/sim_estimates_plot_case1.pdf, output/sim_estimates_plot_case2.pdf, output/sim_estimates_plot_case3.pdf, output/sim_estimates_plot_case4.pdf

- Combined summary table: output/summary_table.pdf

- Combined summary figure: output/summary_figure.pdf

   
