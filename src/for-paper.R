
# --- RESULTS FOR PAPER --------------------------------------------------------
#  Code for producing plots and tables for the paper, based on the estimation
#  results.
# --- READ RESULTS -------------------------------------------------------------

library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(stringi)
library(readxl)
library(forcats)
library(readr)
library(lubridate)
library(tidymodels)
library(here)

result_files <- here("est-results") %>% dir(pattern = "*-results*")

results <- list(tuning_results = list(elastic = tibble(penalty = numeric(),
                                                       mixture = numeric(),
                                                       block = character()),
                                      rf = tibble(mtry = numeric(),
                                                  min_n = numeric(),
                                                  block = character()),
                                      xgb = tibble(mtry = numeric(),
                                                   min_n = numeric(),
                                                   tree_depth = numeric(),
                                                   learn_rate = numeric(),
                                                   loss_reduction = numeric(),
                                                   sample_size = numeric(),
                                                   block = character())),
                variable_importance = tibble(variable = character(),
                                             importance = numeric(),
                                             Sign = character(),
                                             variable_block = numeric(),
                                             model = character(),
                                             block = character()),
                auc = tibble(where = character(),
                             value = numeric(),
                             model = character(),
                             block = character()),
                roc = tibble(.threshold = numeric(),
                             specificity = numeric(),
                             sensitivity = numeric(),
                             where = character(),
                             model = character(),
                             block = character()),
                vi_perm = tibble(variable = character(),
                                 block = character(),
                                 perm_auc = numeric(),
                                 iteration = numeric(),
                                 where = character(),
                                 model = character()))

models <- c("lr", "elastic", "rf", "xgb")

for(i in 1:length(result_files)) {
  
  # Which model has been used?
  model <- sapply(models, function(x) grepl(x, result_files[i])) %>%
    which %>%
    names

  # Load the results
  load(here("est-results", result_files[i]))

  # Save the tuning result
  if(model != "lr") {
    results$tuning_results[[model]] <-
      results$tuning_results[[model]] %>%
      bind_rows(result$tr)
  }

  # Save the auc-result
  results$auc <-
    results$auc %>%
    bind_rows(result$auc)

  # Save the roc-results
  results$roc <-
    results$roc %>%
    bind_rows(result$roc)

  # Save the variable importance results
  results$variable_importance <-
    result$vi %>%
    rename(variable = Variable.x,
           importance = Importance,
           variable_block = Blocks) %>%
    bind_rows(results$variable_importance)

  # Save the permuted variable importance
  if(!is.null(result$vi_perm)) {
    results$vi_perm <-
      result$vi_perm %>%
      bind_rows(results$vi_perm)
  }
}

# --- AUC TABLE FOR LOGISTIC REGRESSION, BLOCK 1 --> 5 -------------------------
options(knitr.kable.NA = "")

results$auc %>%
  # Only on test data
  filter(where == "test") %>%
  # Remove everything to do with block 6
  filter(!grepl("6", block, fixed = TRUE)) %>%
  # Only use logistic regression
  filter(model == "lr") %>%
  pivot_wider(names_from = model) %>%
  # Make column for cumulative addition of blocks
  mutate(cumulative = ifelse(grepl("-", block, fixed = TRUE),
                             yes = "Cumulative addition of blocks",
                             no = "Single block")) %>%
  mutate(block = stri_sub(block, -1)) %>%
  pivot_wider(names_from = cumulative, values_from = lr) %>%
  arrange(block) %>%
  select(-where) %>%
  rename("Variable block" = block) %>%
  select(c(1, 3, 2)) %>%
  kbl(digits = 3, booktabs = TRUE, format = "latex",
      caption = "The AUC for the logistic regression.", label = "auc-lr", ) %>%
  save_kable(file = here("for-paper", "revision_auc_logistic_regression.tex"))

# --- AUC TABLE FOR ALL MODELS, BLOCK 5 ----------------------------------------
options(knitr.kable.NA = "")

results$auc %>%
  # Only on test data
  filter(where == "test") %>%
  # Remove everything to do with block 6
  filter(block == "1-2-3-4-5") %>%
  pivot_wider(names_from = model) %>%
  select(-where, -block) %>%
  rename("Elstic net" = elastic,
         "Logistic regression" = lr,
         "Random forest" = rf,
         "xgBoost" = xgb) %>%
  select(c(2, 1, 3, 4)) %>%
  kbl(digits = 3, booktabs = TRUE, format = "latex",
      caption = "The AUC for the different models using blocks 1 to 5",
      label = "auc-all-models") %>%
  save_kable(file = here("for-paper", "revision_auc_all_models.tex"))

# --- PERMUTED VI PLOTS LOGISTIC REGRESSIOM BLOCK 1 --> 5 ----------------------
vi_perm <-
  results$vi_perm %>%
  # Only on test data
  filter(where == "test") %>%
  # Remove everything to do with block 6
  filter(!grepl("6", block, fixed = TRUE)) %>%
  # Only use logistic regression
  filter(model == "lr") %>%
  # Make column for cumulative addition of blocks
  mutate(cumulative = ifelse(grepl("-", block, fixed = TRUE),
                             yes = "Cumulative addition of blocks",
                             no = "Using the blocks separately")) %>%
  left_join(results$auc) %>%
  rename(full_auc = value) %>%
  mutate(diff_auc = full_auc - perm_auc) %>%
  select(-where, -model) %>%
  mutate(block = stri_sub(block, -1))

vi_aggregated <-
  vi_perm %>%
  group_by(variable, block, cumulative) %>%
  summarise(mean_auc = mean(diff_auc)) %>%
  mutate(block = stri_sub(block, -1))

# Correlact ordering of the blocks
vi_aggregated$block <- factor(vi_aggregated$block, levels = c("1", "2", "3",
                                                              "4", "5"))
vi_aggregated$cumulative <- factor(vi_aggregated$cumulative,
                                   levels = c("Using the blocks separately",
                                              "Cumulative addition of blocks"))
vi_perm$cumulative <- factor(vi_perm$cumulative,
                             levels = c("Using the blocks separately",
                                        "Cumulative addition of blocks"))

vi_aggregated$variable <-
  factor(vi_aggregated$variable,
         levels =
           vi_aggregated %>%
           arrange(block) %>%
           filter(cumulative == "Using the blocks separately") %>%
           select(variable) %>%
           pull %>%
           rev
  )

vi_logistic <-
  vi_aggregated %>%
  ggplot(aes(x = mean_auc, y = variable)) +
  geom_col(position = "dodge", alpha = .7) +
  geom_point(aes(x = diff_auc),
             data = vi_perm,
             position = position_dodge(1),
             alpha = .2) +
  facet_grid(rows = vars(cumulative), cols = vars(block)) +
  xlab("") +
  ylab("") +
  theme_minimal()

ggsave(filename = here("for-paper", "revision_vi_logistic.pdf"),
                       plot = vi_logistic)

# --- PERMUTED VI PLOTS LOGISTIC REGRESSIOM BLOCK 1-2-3-4-5 ------------------
variable_names <- read_csv(here("data", "variable_names.csv"))

update_name <- function(x, variable_names) {
  new_name <- rep(NA, length(x))
  for(i in 1:length(new_name)) {
    new_name[i] <-
      variable_names %>%
      filter(old == x[i]) %>%
      select(new) %>%
      pull
  }
  return(new_name)
}

vi_perm <-
  results$vi_perm %>%
  # Only on test data
  filter(where == "test") %>%
  # Remove everything to do with block 6
  filter(block == "1-2-3-4-5") %>%
  # Only use logistic regression
  filter(model == "lr") %>%
  left_join(results$auc) %>%
  mutate(variable = update_name(variable, variable_names)) %>%
  rename(full_auc = value) %>%
  mutate(diff_auc = full_auc - perm_auc) %>%
  select(-where, -model)

vi_aggregated <-
  vi_perm %>%
  group_by(variable) %>%
  summarise(mean_auc = mean(diff_auc)) %>%
  mutate(variable = factor(variable))

vi_aggregated$variable <- fct_reorder(vi_aggregated$variable,
                                      vi_aggregated$mean_auc)

vi_logistic_1_2_3_4_5 <-
  vi_aggregated %>%
  ggplot(aes(x = mean_auc, y = variable)) +
  geom_col(position = "dodge", alpha = .7) +
  geom_point(aes(x = diff_auc),
             data = vi_perm,
             position = position_dodge(1),
             alpha = .2) +
  xlab("") +
  ylab("") +
  theme_minimal()

ggsave(filename = here("for-paper", "revision_vi_logistic_1_2_3_4_5.pdf"),
       plot = vi_logistic_1_2_3_4_5)

# --- TABLE OF DESCRIPTIVE STATISTICS ------------------------------------------
load(here("est-results", "block1-2-3-4-5-data.Rdata"))
variables <- read_excel(here("data", "variables_revision.xlsx")) %>%
  select(Variable, Blocks) %>%
  rename(variable = Variable,
         block = Blocks) %>%
  drop_na()

variable_names <- read_csv(here("data", "variable_names.csv"))

update_name <- function(x, variable_names) {
  new_name <- rep(NA, length(x))
  for(i in 1:length(new_name)) {
    new_name[i] <-
      variable_names %>%
      filter(old == x[i]) %>%
      select(new) %>%
      pull
  }
  return(new_name)
}

sumstats <-
  litigation_df %>%
  select(-pub_nbr_OECD, -grant_date_PV,
         -litigation_filing_date, -litigated_LIT) %>%
  mutate(pct_PV = as.numeric(litigation_df$pct_PV)-1) %>%
  mutate(foreign_prio_PV = as.numeric(litigation_df$foreign_prio_PV) - 1) %>%
  mutate(assignee_PV = as.numeric(litigation_df$assignee_PV) - 1) %>%
  mutate(lawyer_PV = as.numeric(litigation_df$lawyer_PV) - 1) %>%
  mutate(small_entity_indicator_EX =
         as.numeric(litigation_df$small_entity_indicator_EX) - 1) %>%
  mutate(nber_subcategory_PV = 0) %>%
  mutate(appln_id_OECD = factor(appln_id_OECD)) %>%
  pivot_longer(!appln_id_OECD, names_to = "variable", values_to = "value") %>%
  select(-appln_id_OECD) %>%
  group_by(variable) %>%
  summarise(Observations = sum(complete.cases(value)),
            Mean = mean(value, na.rm = TRUE),
            "Std. Dev" = sd(value, na.rm = TRUE),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE)) %>%
  left_join(variables) %>%
  arrange(block, variable) %>%
  select(c(1, 7, 2:6)) %>%
  rename(Variable = variable,
         Block = block) %>%
  mutate(., Mean = ifelse(.$Variable == "nber_subcategory_PV",
                          yes = rep(NA, nrow(.)),
                          no = .$Mean)) %>%
  mutate(., "Std. Dev" = ifelse(.$Variable == "nber_subcategory_PV",
                                yes = rep(NA, nrow(.)),
                                no = .$`Std. Dev`)) %>%
  mutate(., Min = ifelse(.$Variable == "nber_subcategory_PV",
                         yes = rep(NA, nrow(.)),
                         no = .$Min)) %>%
  mutate(., Max = ifelse(.$Variable == "nber_subcategory_PV",
                         yes = rep(NA, nrow(.)),
                         no = .$Max)) %>%
  mutate(Variable = update_name(Variable, variable_names)) %>%
  kbl(digits = 1,
      booktabs = T,
      format = "latex",
      caption = "Summary statistics for the variables.",
      label = "sumstats") %>%
  save_kable(file = here("for-paper", "revision_summary_statistics.tex"))

# --- ROC CURVE FOR ALL MODELS, BLOCK 5 ----------------------------------------

roc_all_models <-
  results$roc %>%
  filter(where == "test",
         block == "1-2-3-4-5")  %>%
  mutate(model = recode(.$model,
                        xgb = "xgBoost",
                        elastic = "Elastic net",
                        lr = "Logistic regression",
                        rf = "Random forest")) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, colour = model)) +
  geom_line() +
  geom_abline(aes(intercept = 0, slope = 1),
              linetype = 3, alpha = .7) +
  theme_minimal() +
  xlab("False positive rate") +
  ylab("True positive rate") +
  labs(colour = "") +
  theme(legend.position = "bottom")

ggsave(filename = here("for-paper", "revision_roc_all_models.pdf"),
       plot = roc_all_models,
       width = 6, height = 6)

# --- ROC CURVE FOR ALL LOGISTIC, BLOCK 1 -> 5  CUMMULATIVE --------------------

roc_all_blocks <-
  results$roc %>%
  filter(where == "test",
         model == "lr",
         !(block %in% (2:5 %>%
                         as.character)))  %>%
  mutate(model = recode(.$model,
                        xgb = "xgBoost",
                        elastic = "Elastic net",
                        lr = "Logistic regression",
                        rf = "Random forest")) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, colour = block)) +
  geom_line() +
  geom_abline(aes(intercept = 0, slope = 1),
              linetype = 3, alpha = .7) +
  theme_minimal() +
  xlab("False positive rate") +
  ylab("True positive rate") +
  labs(colour = "") +
  theme(legend.position = "bottom")

ggsave(filename = here("for-paper", "revision_roc_all_blocks.pdf"),
       plot = roc_all_blocks,
       width = 6, height = 6)

# --- SMALL SAMPLE EXPERIMENT --------------------------------------------------

load(here("est-results", "revision_small_sample_results.Rdata"))

ss <-
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
  geom_crossbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 3000),
                alpha = .5) +
  ylab("Average AUC") +
  xlab("Sample size") +
  labs(fill = "", colour = "") +
  scale_colour_manual(values = c("black", "grey50")) +
  scale_fill_manual(values = c("black", "grey50")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(filename = here("for-paper", "revision_small_sample.pdf"),
       plot = ss, width = 7, height = 5)

# --- TIME TO LITIGATION FIGURE ------------------------------------------------

# Load the data for all the blocks
load(here("est-results", "block1-2-3-4-5-data.Rdata"))

litigation_lag <-
  litigation_df %>%
  select(grant_date_PV, litigation_filing_date) %>%
  drop_na() %>%
  mutate(lit_lag = litigation_filing_date - grant_date_PV) %>%
  mutate(lit_lag = ifelse(lit_lag < 0, yes = 0, no = lit_lag)) %>%
  ggplot(aes(x = lit_lag / 365.25, y = ..density..)) +
  geom_histogram(binwidth = 1) +
  xlab("Time from patent grant to litigation (Years)") +
  ylab("") +
  theme_minimal()

ggsave(filename = here("for-paper", "revision_time_to_litigation.pdf"),
       plot = litigation_lag)

# --- ODDS RATIO OF THE LOGISTIC REGRESSION MODEL ------------------------------

# Make the logistic regression model for the final blocks
load(here("est-results", "block1-2-3-4-5-data.Rdata"))

# Repeat the modelling steps so that we get the actual model and not just the
# results.
set.seed(1)
litigation_split <- initial_split(litigation_df, strata = litigated_LIT)
litigation_train <- training(litigation_split)
litigation_test  <- testing(litigation_split)

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

# Fit the model
lr_fit <-
  litigation_workflow %>%
  fit(data = litigation_train)

odds_ratio <-
  tidy(lr_fit) %>%
  mutate(odds_ratio = exp(estimate)) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = odds_ratio, y = term)) +
  geom_col() +
  geom_vline(xintercept = 1) +
  xlab("Odds ratio") +
  ylab("Variable") +
  theme_minimal()

ggsave(filename = here("for-paper", "revision_odds_ratio.pdf"),
       plot = odds_ratio)


# CONFUSION MATRIX --------------
results$roc %>%
  filter(model == "xgb", where == "test") %>%
  filter(sensitivity > .75) %>%
  arrange(sensitivity) %>%
  mutate(spec = 1 - specificity) %>%
  pull(spec) %>%
  head

# --> threshold = .0136

# Assume that we have the xgb_fit...

predict(xgb_fit, new_data = litigation_test, type = "prob") %>%
  mutate(true = litigation_test$litigated_LIT) %>%
  mutate(predicted = ifelse(.pred_Litigated > 0.0136,
                            yes = "Litigated",
                            no = "Not litigated")) %>%
  select(true, predicted) %>%
  table()
