# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ROBUSTNESS CHECKS: GENERATE DATA SETS ----------------------------------- #
#
# Function for generating data sets for use in the robustness checks

# temp
# t0 <- 2006
# delta_t1 <- 4
# delta_t2 <- 5
# blocks <- 1:5
# data_file <- "../../data/consolidation_revision.csv"
# output_folder <- "../../data/robustness-datasets"

generate_robustness_dataset <- function(t0, delta_t1, delta_t2, blocks, 
                                        data_file, output_folder) {
  
  
  variables <-
    readxl::read_excel("../../data/variables_revision.xlsx") %>%
    filter(Blocks %in% blocks) %>%
    select(Variable, Blocks)
  
  if(identical(blocks, 1:5)) {
    
    data_file %>%  
      read_csv(col_names = TRUE,
               guess_max = LaF::determine_nlines(.)) %>% 
      # Basic filtering on grant year and case type, check that both of these are 
      # correct
      mutate(grant_date_PV = ymd(grant_date_PV)) %>%
      mutate(grant_year_PV = year(grant_date_PV)) %>%
      mutate(litigation_filing_date = mdy(.$litigation_filing_LIT)) %>%
      filter((case_type_LIT == 1) | is.na(case_type_LIT)) %>%
      filter(grant_year_PV %in% (t0 - delta_t1):(t0 - 1)) %>%
      # Select some ID columns and date columns, the outcome and the explanatory
      # variables for this block
      select(appln_id_OECD, pub_nbr_OECD,            # ID columns
             grant_date_PV, litigation_filing_date,  # Date columns
             all_of(variables$Variable),             # Explanatory variables
             litigated_LIT) %>%                      # The outcome variable
      # Convert to factor variables
      mutate(litigated_LIT = case_when(litigated_LIT == 0 ~ "Not litigated",
                                       TRUE ~ "Litigated") %>% factor)  %>%
      # Calculate the time lag between grant date and litigation filing date.
      # Rounds down to the nearest year, so we can do a strict less than 
      # afterwards to find all patents with litigation lag less than this year.
      mutate(litigation_lag = 
               interval(grant_date_PV,litigation_filing_date) %>% 
               as.period() %>% 
               year() 
             ) %>% 
      # Create the new outcome variable
      mutate(litigated_by = ifelse(litigation_lag < delta_t2,
                                   yes = "Litigated",
                                   no  = "Not litigated")) %>% 
      replace_na(list(litigated_by = "Not litigated")) %>% 
      mutate(litigated_by = litigated_by %>% factor) %>% 
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
      drop_na(litigated_LIT, all_of(variables$Variable)) %>% 
      write_csv(file = paste0(output_folder, "/robustness-", t0, ".csv"))
    
    
  } else {
    cat("\n Generation of robustness data sets only works for blocks 1:5 \n")
  }
}