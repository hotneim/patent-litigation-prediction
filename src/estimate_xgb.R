
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ESTIMATE  AND SAVE RANDOM FOREST  --------------------------------- #

cat("\n\n -- XGBOOST ---------------------------------------------------- \n\n")

# Specify the logistic regression model 
xgb_mod <- boost_tree(
  trees = 100, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Set up the workflows
litigation_workflow <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_recipe(litigation_recipe)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), litigation_train),
  learn_rate(),
  size = GRID
)

cat("  Tuning the hyperparameters ... \n")

xgb_tune_result <- 
  tune_grid(
    litigation_workflow,
    resamples = litigation_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )

tr <- 
  xgb_tune_result %>%
  select_best(metric = "roc_auc") %>%
  select(-.config) %>%
  mutate(block = paste(BLOCK, collapse = "-"))

xgb_tuned <- 
  finalize_workflow(
    x = litigation_workflow,
    parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
  )

cat("  Fitting the model ... \n")

# Fit the model
xgb_fit <- 
  xgb_tuned %>% 
  fit(data = litigation_train)

# Evaluate the results
xgb_result <- evaluate(fit = xgb_fit,
                      training_data = litigation_train,
                      testing_data = litigation_test,
                      model = "xgb")

# Calculate the variable importance
xgb_vi <- 
  xgb_fit %>%
  pull_workflow_fit() %>%
  vi() %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>%
  regex_left_join(variables, by = "Variable") %>% 
  select(-Variable.y) %>% 
  mutate(model = "xgb",
         block = paste(BLOCK, collapse = "-"))

cat("  Calculating permuted variable importance ... \n")

# Calculate the permuted variable importance
xgb_vi_perm <- permute_vi(wflow = xgb_tuned, 
                         training_data = litigation_train, 
                         testing_data  = litigation_test, 
                         model = "xgb", 
                         variables = variables, 
                         nsim = NSIM)

# Collect the results in a list
result <- list(auc     = xgb_result$auc,
               roc     = xgb_result$roc,
               vi      = xgb_vi,
               vi_perm = xgb_vi_perm,
               tr      = tr)

# Save the list
save(result, file = paste0(file_base, "-results-xgb.Rdata"))
