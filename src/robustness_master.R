# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ROBUSTNESS CHECKS                 --------------------------------------- #
#
# We run a series of robustness checks where we redefine the outcome to a patent
# being litigated within delta_t2 years, and run some models for this across
# time.


# ---- SET PARAMETERS -------------------------------------------------------- #
BLOCK <- 1:5 # Which blocks of variables to use
T0 <- 2006:2016 # Starting year
NSIM <- 15 # How many iterations for calculating the permuted VI
GRID <- 100 # Size of grid to tune the models
DELTA_T1 <- 4 # Number of years in observation period
DELTA_T2 <- 5 # Litigated within

# ---- LOAD PACKAGES AND GENERAL SOURCE FILES -------------------------------- #
library(tidyverse)
library(tidymodels)
library(lubridate)
library(fuzzyjoin)
library(kableExtra)
library(glmnet)
library(doParallel)

# ---- CREATE DATA SETS FOR EACH CHECK --------------------------------------- #

source("robustness_utils.R")

# This part requires the full data set which runs into the gigabytes, and we
# are unable to host this file on github. Available upon request.

# for(t0 in 2006:2015) {
#   generate_robustness_dataset(t0 = t0, delta_t1 = 4, delta_t2 = 5, blocks = 1:5,
#                               data_file = "../../data/consolidation_revision.csv",
#                               output_folder = "../../data/robustness-datasets")
# }
#
