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
├── 0_setup.R # Package installation and environment setup 
├── functions.R # Helper functions (export_est, etc.) 
├── dta/ # Simulated data files 
├── output/ # PDF tables and figures 
├── renv/ # Managed package environment (created by renv) 
├── renv.lock # Lockfile for package versions 
├── scripts/ # R scripts for each scenario and comparisons │ 
├── Version1.R  
├── Version2.R  
├── Version3.R  
├── Version4.R  
├── comparison_across_scenarios.R 
├── main.R # Master script that runs all analyses 
├── README.md # Project documentation

## Requirements

- **R 4.3.1 or higher**
- **renv** – used for package management (activate by running `renv::restore()`)
- The following R packages (versions recorded in `renv.lock`): dplyr, ggplot2, multe, tidyr, readstata13, remotes, testthat, gridExtra, and grid.

## Instructions

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/<your_username>/ContaminationBiasSim.git
   cd ContaminationBiasSim
   
2. **Restore the R Environment:**

   In R (or RStudio), run:
   install.packages("renv")
renv::restore()

   
