
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ESTIMATE  AND SAVE RANDOM FOREST  --------------------------------- #

cat("\n\n -- RANDOM FOREST----------------------------------------------- \n\n")

# Specify the random forest model
rf_mod <-
  rand_forest(
    mtry = tune(),
    trees = 100,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")

# Set up the workflows
litigation_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(litigation_recipe)

rf_grid <- grid_latin_hypercube(
  min_n(),
  finalize(mtry(), litigation_train),
  size = GRID
)

cat("  Tuning the hyperparameters ... \n")

rf_tune_result <-
  tune_grid(
    litigation_workflow,
    resamples = litigation_folds,
    grid = rf_grid,
    control = control_grid(save_pred = TRUE)
  )

tr <-
  rf_tune_result %>%
  select_best(metric = "roc_auc") %>%
  select(-.config) %>%
  mutate(block = paste(BLOCK, collapse = "-"))

rf_tuned <-
  finalize_workflow(
    x = litigation_workflow,
    parameters = rf_tune_result %>% select_best(metric = "roc_auc")
  )

cat("  Fitting the model ... \n")

# Fit the model
rf_fit <-
  rf_tuned %>%
  fit(data = litigation_train)

cat("  Evaluating the results ... \n")

# Evaluate the results
rf_result <- evaluate(fit = rf_fit,
                      training_data = litigation_train,
                      testing_data = litigation_test,
                      model = "rf")

# Calculate the variable importance
rf_vi <-
  rf_fit %>%
  pull_workflow_fit() %>%
  vi() %>%
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>%
  regex_left_join(variables, by = "Variable") %>%
  select(-Variable.y) %>%
  mutate(model = "rf",
         block = paste(BLOCK, collapse = "-")) %>%
  mutate(importance_measure = "permute")

cat("  Calculating permuted variable importance ... \n")

# Calculate the permuted variable importance
rf_vi_perm <- permute_vi(wflow = rf_tuned,
                         training_data = litigation_train,
                         testing_data  = litigation_test,
                         model = "rf",
                         variables = variables,
                         nsim = NSIM)

# Collect the results in a list
result <- list(auc     = rf_result$auc,
               roc     = rf_result$roc,
               vi      = rf_vi,
               vi_perm = rf_vi_perm,
               tr      = tr)

# Save the list
save(result,
     file = here("est-results", paste0(file_base, "-results-rf.Rdata")))
