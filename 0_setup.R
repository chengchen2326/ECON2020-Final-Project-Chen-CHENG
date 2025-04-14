#############################
# 0_setup.R
#############################
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
packages <- c("dplyr", "ggplot2", "multe", "tidyr", "readstata13", "remotes", "testthat", "gridExtra", "grid")
lapply(packages, pkgTest)

if (!require("ggthemr")) {
  remotes::install_github("Mikata-Project/ggthemr")
  library(ggthemr)
}
