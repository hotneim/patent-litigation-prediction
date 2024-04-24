
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- MASTER FILE                       --------------------------------------- #
#
# This file is where we set all the parameters for estimating the models and
# producing the results.

# We use the here package to organize the file structure. Defines the parent
# folder of the git repository as the root path.
library(here)

# ---- SET PARAMETERS -------------------------------------------------------- #
BLOCK <- 1:5              # Which blocks of variables to use
YEARS <- 2002:2005        # Which years to consider
CLEAN <- F                # Run the part where we build the cleaned-up data set?
                          # (Not necessary to run unless starting from scratch,
                          # because the cleaned up data file is available in
                          # the repository.)
NSIM  <- 15               # How many iterations for calculating the permuted VI
GRID  <- 100              # Size of grid to tune the models

# ---- LOAD PACKAGES AND GENERAL SOURCE FILES -------------------------------- #
library(tidyverse)
library(tidymodels)
library(readxl)
library(lubridate)
library(vip)
library(fuzzyjoin)
library(patchwork)
library(kableExtra)
library(glmnet)
library(doParallel)
library(themis)

here("src", "evaluate.R") %>% source()
here("src", "permute_vi.R") %>% source()

# Parallel computing
# all_cores <- parallel::detectCores(logical = TRUE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)

# ---- SOURCE THE BLOCK-SPECIFIC SCRIPT -------------------------------------- #
here("src", paste0("block", paste(BLOCK, collapse = "-"), ".R")) %>% source

# ---- FIT AND SAVE THE VARIOUS MODELS --------------------------------------- #

{
 here("src", "estimate_lr.R")               %>% source
 here("src", "estimate_elastic.R")          %>% source
 here("src", "estimate_rf.R")               %>% source
 here("src", "estimate_xgb.R")              %>% source
}
