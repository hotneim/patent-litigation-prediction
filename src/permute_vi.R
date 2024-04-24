# -- FUNCTION FOR CALCULATING THE PERMUATATION VARIABLE IMPORTANCE -------------

permute_vi <- function(wflow, training_data, testing_data, model, variables, 
                       nsim) {

  permuted_auc <- tibble(variable = character(),
                         block = character(),
                         perm_auc = numeric(),
                         iteration = numeric(),
                         where = character(),
                         model = character())
  
  cat("\n")
    
  # Loop over the variables, permute them and calculate the AUC
  for(i in 1:nrow(variables)) {
    
    variable <- variables$Variable[i]
    
    cat("   Variable (")
    cat(i)
    cat("/")
    cat(nrow(variables))
    cat("): ")
    cat(variable)
    cat("\n     Iteration: ")
    
    for(j in 1:nsim) {
      
      cat(j); cat(" ")
      
      # Permute the training and testing data
      train_perm <- 
        training_data %>% 
        mutate(!! variable := sample(!! as.name(variable)))
      
      # Fit the model to the permuted data
      perm_fit <- wflow %>% fit(data = train_perm)
      
      # Calculate the predictive performance of the permuted fit
      permuted_auc <-
        permuted_auc %>% 
        bind_rows(
          evaluate(perm_fit, training_data, testing_data, model = model)$auc %>% 
            rename(perm_auc = value) %>% 
            mutate(model = model,
                   iteration = j,
                   variable = variable))
        
    }
    
    cat("\n")
    
  }

  return(permuted_auc)
  
}


