
# -- FUNCTION FOR EVALUATING THE FINAL RESULT ----------------------------------

evaluate <- function(fit, training_data, testing_data, model) {
  
  # Initialize
  auc <- tibble(where = character(),
                value = numeric())
  
  roc <- tibble(.threshold = numeric(),
                specificity = numeric(),
                sensitivity = numeric(),
                where = character())
  
  
  # -- COLLECT THE AUC -------------------------------------------------------- #
  predictions_training <- 
    fit %>% 
      predict(new_data = training_data,
              type = "prob") %>% 
      mutate(truth = training_data$litigated_LIT)
    
  predictions_testing <- 
    fit %>% 
    predict(new_data = testing_data,
              type = "prob") %>% 
    mutate(truth = testing_data$litigated_LIT)
    
  auc <- 
    auc %>% 
    bind_rows(
      tibble(where = "train",
             value = predictions_training %>% 
               roc_auc(truth, .pred_Litigated) %>% 
               select(.estimate) %>% 
               pull)) %>% 
    bind_rows(
      tibble(where = "test",
             value = predictions_testing %>% 
               roc_auc(truth, .pred_Litigated) %>% 
               select(.estimate) %>% 
               pull)) %>% 
    mutate(model = model,
           block = paste(BLOCK, collapse = "-"))
    
  # -- DATA FOR PLOTTING THE ROC --------------------------------------------- #
  roc <- 
    roc %>% 
    bind_rows(predictions_training %>% 
                roc_curve(truth, .pred_Litigated) %>% 
                mutate(where = "train"))  %>% 
    bind_rows(predictions_testing %>% 
                roc_curve(truth, .pred_Litigated) %>% 
                mutate(where = "test")) %>% 
    mutate(model = model,
           block = paste(BLOCK, collapse = "-"))
  
  # Return the AUC and the ROC data
  return(list(auc = auc, roc = roc))
  
}
