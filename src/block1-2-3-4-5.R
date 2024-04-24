
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- BLOCK 1 + 2 + 3 + 4 + 5           --------------------------------------- #

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


# -- LOAD AND CLEAN THE FULL DATA SET ---------------------------------------- #
# This is only necessary to run the first time to create a version of the 
# data set that only contains the relevant observations and variables that is
# much quicker to load. The resulting cleaned up data set is included in this
# repository.

if(CLEAN) {
  
  litigation_df <-
    here("data", "consolidation_revision.csv") %>%
    readr::read_csv(., col_names = TRUE,
                    guess_max = LaF::determine_nlines(.)) %>%
    # Basic filtering on grant year and case type, check that both of these are 
    # correct
    mutate(grant_date_PV = ymd(grant_date_PV)) %>%
    mutate(grant_year_PV = year(grant_date_PV)) %>%
    mutate(litigation_filing_date = mdy(.$litigation_filing_LIT)) %>%
    filter((case_type_LIT == 1) | is.na(case_type_LIT)) %>%
    filter(grant_year_PV %in% YEARS) %>%
    # Select some ID columns and date columns, the outcome and the explanatory
    # variables for this block
    select(appln_id_OECD, pub_nbr_OECD,            # ID columns
           grant_date_PV, litigation_filing_date,  # Date columns
           all_of(variables$Variable),             # Explanatory variables
           litigated_LIT) %>%                      # The outcome variable
    # Convert to factor variables
    mutate(litigated_LIT = case_when(litigated_LIT == 0 ~ "Not litigated",
                                     TRUE ~ "Litigated") %>% factor)  %>%
    # Convert block-specific variables
    mutate(nber_subcategory_PV = factor(nber_subcategory_PV)) %>%
    mutate(pct_PV = factor(pct_PV)) %>%
    mutate(foreign_prio_PV = factor(foreign_prio_PV)) %>% 
    mutate(assignee_PV = factor(assignee_PV)) %>%
    mutate(lawyer_PV = factor(lawyer_PV)) %>% 
    mutate(small_entity_indicator_EX = factor(small_entity_indicator_EX)) %>% 
    # NB! Added this after the revision. This is for creating the data file that
    # will be used for all blocks and combination of blocks that are less than 
    # the full block 1-5. Therefore, all combinations will use the same 
    # observations.
    drop_na(litigated_LIT, all_of(variables$Variable))
  
    # Save the reduced version of the data set in the RMD-folder
    save(litigation_df, file = here("data", paste0(file_base, "-data.Rdata")))
  
}

# Load the data relevant for this block
load(paste0(file_base, "-data.Rdata"))

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
set.seed(1)
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
  # Lump together the factor variable
  step_other(nber_subcategory_PV, threshold = 0.03) %>% 
  # One hot encoding of the categorical
  step_dummy(all_nominal(), -all_outcomes(), -pub_nbr_OECD) 

