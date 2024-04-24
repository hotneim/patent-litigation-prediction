
# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ESTIMATE  AND SAVE LOGISTIC REGRESSION  --------------------------------- #

cat("\n\n -- LOGISTIC REGRESSION ---------------------------------------- \n\n")

# Specify the logistic regression model 
lr_mod <- 
  logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

# Set up the workflows
litigation_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(litigation_recipe)

cat("  Fitting the model ... \n")

# Fit the model
lr_fit <- 
  litigation_workflow %>% 
  fit(data = litigation_train)

cat("  Evaluating the results ... \n")

# Evaluate the results
lr_result <- evaluate(fit = lr_fit,
                      training_data = litigation_train,
                      testing_data = litigation_test,
                      model = "lr")

# Calculate the variable importance
lr_vi <- 
  lr_fit %>%
  pull_workflow_fit() %>%
  vi() %>% 
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance)) %>%
  regex_left_join(variables, by = "Variable") %>% 
  select(-Variable.y) %>% 
  mutate(model = "lr",
         block = paste(BLOCK, collapse = "-")) 

cat("  Calculating permuted variable importance ... \n")

# Calculate the permuted variable importance
lr_vi_perm <- permute_vi(wflow = litigation_workflow, 
                         training_data = litigation_train, 
                         testing_data  = litigation_test, 
                         model = "lr", 
                         variables = variables, 
                         nsim = NSIM)

# Collect the results in a list
result <- list(auc     = lr_result$auc,
               roc     = lr_result$roc,
               vi      = lr_vi,
               vi_perm = lr_vi_perm,
               tr      = NA)

# Save the list
save(result, file = paste0(file_base, "-results-lr.Rdata"))
