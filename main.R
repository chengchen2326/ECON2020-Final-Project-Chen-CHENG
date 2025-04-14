#############################
# main.R
#############################
# Run the complete analysis: all cases and the summary comparison

# Source the setup script first.
source("0_setup.R")
source("functions.R")

# Run each scenario script (order matters if they generate files in the output)
source("Case1.R")
source("Case2.R")
source("Case3.R")
source("Case4.R")

# Combine and produce summary outputs (tables and figures in PDF)
source("comparison_across_cases.R")

cat("All analyses completed. Check the 'output' folder for PDF tables and figures.\n")
