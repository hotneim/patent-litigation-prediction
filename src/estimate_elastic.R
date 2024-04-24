
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ESTIMATE  AND SAVE ELASTIC NET          --------------------------------- #

cat("\n\n -- ELASTIC NETS ----------------------------------------------- \n\n")

# Specify the elastic net 
elastic_mod <- 
  logistic_reg(penalty = tune(),
               mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

# Set up the workflows
litigation_workflow <- 
  workflow() %>% 
  add_model(elastic_mod) %>% 
  add_recipe(litigation_recipe)

cat("  Tuning the hyperparameters ... \n")

# Tune the hyperparameters
elastic_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = GRID
)

elastic_tune_result <- 
  tune_grid(
    litigation_workflow,
    resamples = litigation_folds,
    grid = elastic_grid,
    control = control_grid(save_pred = TRUE)
  )

tr <- 
  elastic_tune_result %>%
  select_best(metric = "roc_auc") %>%
  select(-.config) %>%
  select(penalty, mixture) %>% 
  mutate(block = paste(BLOCK, collapse = "-"))
  
elastic_tuned <- 
  finalize_workflow(
    x = litigation_workflow,
    parameters = elastic_tune_result %>% select_best(metric = "roc_auc")
  )

cat("  Fitting the model ... \n")

# Fit the model
elastic_fit <- 
  elastic_tuned %>% 
  fit(data = litigation_train)

cat("  Evaluating the results ... \n")

# Evaluate the results
elastic_result <- evaluate(fit = elastic_fit,
                      training_data = litigation_train,
                      testing_data = litigation_test,
                      model = "elastic")

# Calculate the variable importance
lambda <- elastic_tune_result %>%
  select_best(metric = "roc_auc") %>%
  select(penalty) %>%
  pull

elastic_vi <- 
  pull_workflow_fit(elastic_fit)$fit %>%
  vi_model(s = lambda) %>%
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>%
  regex_left_join(variables, by = "Variable") %>% 
  select(-Variable.y) %>% 
  mutate(model = "elastic",
         block = paste(BLOCK, collapse = "-"))

cat("  Calculating permuted variable importance ... \n")

# Calculate the permuted variable importance
elastic_vi_perm <- permute_vi(wflow = elastic_tuned, 
                              training_data = litigation_train, 
                              testing_data  = litigation_test, 
                              model = "elastic", 
                              variables = variables, 
                              nsim = NSIM)


# Collect the results in a list
result <- list(auc     = elastic_result$auc,
               roc     = elastic_result$roc,
               vi      = elastic_vi,
               vi_perm = elastic_vi_perm,
               tr      = tr)

# Save the list
save(result, file = paste0(file_base, "-results-elastic.Rdata"))
