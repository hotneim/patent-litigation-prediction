
# --- SMALL SAMPLE ANALYSIS-----------------------------------------------------
# 
# Code, based on the markdown file, for producing plots and tables for the 
# paper.

# For each model and for each sample size we do a number of experiments where we
# sample from the data, fit the model, and measure the AUC. We then create some
# box plots and judge the stability of the predictions. We do this for the
# specific case of block 1 to 5.

# --- Parameters ---------------------------------------------------------------
n_runs             <- 100
sample_sizes       <- c(5000, 10000, 50000, 100000, 250000)

# --- Initialization -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(lubridate)
library(extrafont)
library(vip)
library(fuzzyjoin)
library(patchwork)
library(kableExtra)
library(firatheme)
library(glmnet)
library(doParallel)

source("evaluate.R")

all_cores <- parallel::detectCores(logical = TRUE) - 1
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

set.seed(1)

# Load the data and pre-process-------------------------------------------------
load("../rmd/objects/block1-2-3-4-5-data.Rdata")

BLOCK <- 1:5
GRID  <- 100

variables <- 
  readxl::read_excel("../../data/variables.xlsx") %>% 
  filter(Blocks %in% BLOCK) %>% 
  select(Variable, Blocks)

litigation_df <- 
  litigation_df %>% 
  drop_na(litigated_LIT, all_of(variables$Variable))

# Make initial split as well as folds for cross validation

# Initialize the results -------------------------------------------------------
small_sample_results <- tibble(model = character(),
                               sample_size = numeric(),
                               run = numeric(),
                               n_positives = numeric(),
                               auc = numeric())

for(i in sample_sizes) {
  for(j in 1:n_runs) {
    
    # Make subsample and prepare the data
    subsample <- sample_n(litigation_df, i, replace = FALSE)
    
    litigation_split <- initial_split(subsample, strata = litigated_LIT)
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
    
    # --- LOGISTIC REGRESSION --------------------------------------------------
    
    cat(paste("Logistic regression, sample size", i, ", run", j, " ... "))
    
    lr_mod <- 
      logistic_reg() %>% 
      set_mode("classification") %>% 
      set_engine("glm")
    
    # Set up the workflows
    litigation_workflow <- 
      workflow() %>% 
      add_model(lr_mod) %>% 
      add_recipe(litigation_recipe)
    
    lr_fit <- 
      litigation_workflow %>% 
      fit(data = litigation_train)
    
    lr_result <- evaluate(fit = lr_fit,
                          training_data = litigation_train,
                          testing_data = litigation_test,
                          model = "lr")
    
    small_sample_results <- 
      small_sample_results %>% 
      bind_rows(tibble(model = "lr", 
                       sample_size = i, 
                       run = j, 
                       n_positives = subsample %>% select(litigated_LIT) %>% pull %>% table %>% .["Litigated"],
                       auc = lr_result$auc %>% filter(where == "test") %>%  select(value) %>% pull))
    cat("FINISHED! \n")
    
   #  # --- ELASTIC NETS ---------------------------------------------------------
   #  cat(paste("Elastic net, sample size", i, ", run", j, " ... "))
   #  
   #  elastic_mod <- 
   #    logistic_reg(penalty = tune(),
   #                 mixture = tune()) %>% 
   #    set_mode("classification") %>% 
   #    set_engine("glmnet")
   #  
   #  # Set up the workflows
   #  litigation_workflow <- 
   #    workflow() %>% 
   #    add_model(elastic_mod) %>% 
   #    add_recipe(litigation_recipe)
   #  
   #  elastic_grid <- grid_latin_hypercube(
   #    penalty(),
   #    mixture(),
   #    size = GRID
   #  )
   #  
   #  elastic_tune_result <- 
   #    tune_grid(
   #      litigation_workflow,
   #      resamples = litigation_folds,
   #      grid = elastic_grid,
   #      control = control_grid(save_pred = TRUE)
   #    )
   #  
   #  tr <- 
   #    elastic_tune_result %>%
   #    select_best(metric = "roc_auc") %>%
   #    select(-.config) %>%
   #    select(penalty, mixture) %>% 
   #    mutate(block = paste(BLOCK, collapse = "-"))
   #  
   #  elastic_tuned <- 
   #    finalize_workflow(
   #      x = litigation_workflow,
   #      parameters = elastic_tune_result %>% select_best(metric = "roc_auc")
   #    )
   #  
   #  elastic_fit <- 
   #    elastic_tuned %>% 
   #    fit(data = litigation_train)
   #  
   #  elastic_result <- evaluate(fit = elastic_fit,
   #                             training_data = litigation_train,
   #                             testing_data = litigation_test,
   #                             model = "elastic")
   #  
   #  small_sample_results <- 
   #    small_sample_results %>% 
   #    bind_rows(tibble(model = "elastic", 
   #                     sample_size = i, 
   #                     run = j, 
   #                     n_positives = subsample %>% select(litigated_LIT) %>% pull %>% table %>% .["Litigated"],
   #                     auc = elastic_result$auc %>% filter(where == "test") %>%  select(value) %>% pull))
   #  
   #  cat("FINISHED! \n")
   #  
   #  # --- RANDOM FOREST --------------------------------------------------------
   #  cat(paste("Random forest, sample size", i, ", run", j, " ... "))
   #  
   #  # Specify the random forest model 
   #  rf_mod <-
   #    rand_forest(
   #      mtry = tune(),
   #      trees = 100,
   #      min_n = tune()
   #    ) %>% 
   #    set_mode("classification") %>% 
   #    set_engine("ranger", importance = "impurity")
   #  
   #  # Set up the workflows
   #  litigation_workflow <- 
   #    workflow() %>% 
   #    add_model(rf_mod) %>% 
   #    add_recipe(litigation_recipe)
   #  
   #  rf_grid <- grid_latin_hypercube(
   #    min_n(),
   #    finalize(mtry(), litigation_train),
   #    size = GRID
   #  )
   #  
   # rf_tune_result <- 
   #    tune_grid(
   #      litigation_workflow,
   #      resamples = litigation_folds,
   #      grid = rf_grid,
   #      control = control_grid(save_pred = TRUE)
   #    )
   #  
   #  tr <- 
   #    rf_tune_result %>%
   #    select_best(metric = "roc_auc") %>%
   #    select(-.config) %>%
   #    mutate(block = paste(BLOCK, collapse = "-"))
   #  
   #  rf_tuned <- 
   #    finalize_workflow(
   #      x = litigation_workflow,
   #      parameters = rf_tune_result %>% select_best(metric = "roc_auc")
   #    )
   #  
   # 
   #  # Fit the model
   #  rf_fit <- 
   #    rf_tuned %>% 
   #    fit(data = litigation_train)
   #  
   #  # Evaluate the results
   #  rf_result <- evaluate(fit = rf_fit,
   #                        training_data = litigation_train,
   #                        testing_data = litigation_test,
   #                        model = "rf")
   #  
   #  small_sample_results <- 
   #    small_sample_results %>% 
   #    bind_rows(tibble(model = "rf", 
   #                     sample_size = i, 
   #                     run = j, 
   #                     n_positives = subsample %>% select(litigated_LIT) %>% pull %>% table %>% .["Litigated"],
   #                     auc = rf_result$auc %>% filter(where == "test") %>%  select(value) %>% pull))
   #  
   #  cat("FINISHED! \n")
   #  
   #  # --- xgBoost --------------------------------------------------------------
   #  cat(paste("xgBoost, sample size", i, ", run", j, " ... "))
   #  
   #  xgb_mod <- boost_tree(
   #    trees = 100, 
   #    tree_depth = tune(), min_n = tune(), 
   #    loss_reduction = tune(),                     ## first three: model complexity
   #    sample_size = tune(), mtry = tune(),         ## randomness
   #    learn_rate = tune(),                         ## step size
   #  ) %>% 
   #    set_engine("xgboost") %>% 
   #    set_mode("classification")
   #  
   #  # Set up the workflows
   #  litigation_workflow <- 
   #    workflow() %>% 
   #    add_model(xgb_mod) %>% 
   #    add_recipe(litigation_recipe)
   #  
   #  xgb_grid <- grid_latin_hypercube(
   #    tree_depth(),
   #    min_n(),
   #    loss_reduction(),
   #    sample_size = sample_prop(),
   #    finalize(mtry(), litigation_train),
   #    learn_rate(),
   #    size = GRID
   #  )
   # 
   #  xgb_tune_result <- 
   #    tune_grid(
   #      litigation_workflow,
   #      resamples = litigation_folds,
   #      grid = xgb_grid,
   #      control = control_grid(save_pred = TRUE)
   #    )
   #  
   #  tr <- 
   #    xgb_tune_result %>%
   #    select_best(metric = "roc_auc") %>%
   #    select(-.config) %>%
   #    mutate(block = paste(BLOCK, collapse = "-"))
   #  
   #  xgb_tuned <- 
   #    finalize_workflow(
   #      x = litigation_workflow,
   #      parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
   #    )
   # 
   #  # Fit the model
   #  xgb_fit <- 
   #    xgb_tuned %>% 
   #    fit(data = litigation_train)
   #  
   #  # Evaluate the results
   #  xgb_result <- evaluate(fit = xgb_fit,
   #                         training_data = litigation_train,
   #                         testing_data = litigation_test,
   #                         model = "xgb")
   #  
   #  small_sample_results <- 
   #    small_sample_results %>% 
   #    bind_rows(tibble(model = "xgb", 
   #                     sample_size = i, 
   #                     run = j, 
   #                     n_positives = subsample %>% select(litigated_LIT) %>% pull %>% table %>% .["Litigated"],
   #                     auc = xgb_result$auc %>% filter(where == "test") %>%  select(value) %>% pull))
   #  
   #  cat("FINISHED! \n")
  }
}

save(small_sample_results, file = "../rmd/objects/revision_small_sample_results.Rdata")

load("../rmd/objects/revision_small_sample_results.Rdata")

small_sample <- 
  small_sample_results %>% 
  filter(model %in% c("lr")) %>% 
  mutate(model = recode(.$model, 
                        lr = "Logistic regression",
                        xgb = "xgBoost")) %>% 
  group_by(model, sample_size) %>% 
  summarize(mean = mean(auc),
            sd = sd(auc)) %>% 
  ggplot(aes(x = sample_size, y = mean, colour = model, fill = model)) +
  geom_line(position = position_dodge(width = 3000)) +
  geom_crossbar(aes(ymin = mean - sd, ymax = mean + 2*sd), 
                position = position_dodge(width = 3000),
                alpha = .5) +
  ylab("Average AUC") +
  xlab("Sample size") +
  labs(fill = "", colour = "") +
  scale_colour_manual(values = c("black", "grey50")) +
  scale_fill_manual(values = c("black", "grey50")) +
  theme_minimal() +
  theme(legend.position = "none")


