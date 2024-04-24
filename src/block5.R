
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- BLOCK 5                           --------------------------------------- #

# -- BASIC PREPARATION ------------------------------------------------------- #

# Load information about the variables
variables <- 
  here("data", "variables_revision.xlsx") %>%
  read_excel() %>% 
  filter(Blocks %in% BLOCK) %>% 
  select(Variable, Blocks)

# Create a basic base for the file name
file_base <- here("est-results", 
                  paste0("../rmd/objects/block",
                    paste(BLOCK, collapse = "-")))

# Load the data set. This is a cleaned up version of the data containing only
# the explanatory variables used for this study, and only complete records. The
# script for creating this data from raw data is in the block-specific script 
# for all the blocks.
load(here("data", "block1-2-3-4-5-data.Rdata"))

# Select some ID columns and date columns, the outcome and the explanatory
# variables for this block
litigation_df <- 
  litigation_df %>% 
  select(appln_id_OECD, pub_nbr_OECD,            # ID columns
         grant_date_PV, litigation_filing_date,  # Date columns
         all_of(variables$Variable),             # Explanatory variables
         litigated_LIT)

# Make initial split as well as folds for cross validation
set.seed(1) # For reproducibility
litigation_split <- initial_split(litigation_df, strata = litigated_LIT)
litigation_train <- training(litigation_split)
litigation_test  <- testing (litigation_split)

litigation_folds <- vfold_cv(litigation_train, strata = litigated_LIT)

# Create recipe for training the models
litigation_recipe <-
  recipe(litigated_LIT ~ ., data = litigation_train) %>% 
  # Update roles of the ID and date variables
  update_role(appln_id_OECD, new_role = "ID") %>% 
  update_role(pub_nbr_OECD, new_role = "ID") %>% 
  update_role(litigation_filing_date, new_role = "Date") %>% 
  update_role(grant_date_PV, new_role = "Date") %>% 
  step_normalize(all_numeric(), -appln_id_OECD) %>% 
  step_zv(all_numeric()) %>% 
  # One hot encoding of the categorical
  step_dummy(all_nominal(), -all_outcomes(), -pub_nbr_OECD) 

