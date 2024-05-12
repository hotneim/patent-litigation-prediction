#
# Create figure for illustrating ML-methods
#

set.seed(5)

library(tidyverse)
library(tidymodels)

# Simulate a bivariate labelled data set that can be used for illustration
# purposes.

n       <- 300        # Number of observations in each population
mu2     <- 4          # Shift in the y-direction in the first population
sigma1  <- 1.2        # Noise in the x-direction in the second population
sigma2  <- .75        # Noise in the y-direction in the second population

dat1 <- tibble(x1 = rnorm(n), x2 = mu2 + rnorm(n), pop = "Population 1")
dat2 <-
  tibble(x1 = sigma1 * rnorm(n)) %>%
  mutate(x2 = x1^2 + sigma2 * rnorm(n),
         pop = "Population 2")

x <- bind_rows(dat1, dat2)

# Build classification model using lr and xgb

# LR ----

ill_recipe <-
  recipe(pop ~ ., data = x)

# Specify the decistion tree
ill_mod <-
  logistic_reg() %>%
  set_mode("classification")

# Set up the workflow
ill_workflow <-
  workflow() %>%
  add_model(ill_mod) %>%
  add_recipe(ill_recipe)

fitted_lr <-
  ill_workflow %>%
  fit(data = x)

# XGB -----

ill_folds <- vfold_cv(x, v = 2)

xgb_mod <-
  boost_tree(
    trees = 1000,
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(), mtry = tune(),
    learn_rate = tune(),
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

# Set up the workflow
xgb_workflow <-
  workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(ill_recipe)

# Make a search grid for the parameters
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), x),
  learn_rate(),
  size = 30
)

# Calculate the cross-validated AUC for all the parameter combinations in the
# grid
xgb_tune_result <-
  tune_grid(
    xgb_workflow,
    resamples = ill_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )

# Which parameter combination is the best?
xgb_tune_result %>%
  select_best(metric = "roc_auc")

# Put the best parameters in the workflow
xgb_tuned <-
  finalize_workflow(
    xgb_workflow,
    parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_xgb <-
  xgb_tuned %>%
  fit(data = x)

# Make a plotting grid and predictions ----

cutoff <- .5
grid_resolution <- 100

newdata <-
  expand.grid(x1 = seq(from = -6,
                       to = 6,
                       length.out = grid_resolution),
              x2 = seq(from = -6,
                       to = 15,
                       length.out = grid_resolution))

predictions <-
  newdata %>%
  bind_cols(pred_prob_logistic = predict(fitted_lr,
                                         new_data = newdata,
                                         type = "prob")$`.pred_Population 1`) %>%
  bind_cols(pred_prob_xgb = predict(fitted_xgb,
                                    new_data = newdata,
                                    type = "prob")$`.pred_Population 1`)

# Make a plot -----

scatter <-
  ggplot(x, aes(x = x1, y = x2, shape = pop)) +
  geom_point() +
  scale_shape_manual(values = c(16, 3)) +
  xlab("") +
  ylab("") +
  labs(shape = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ill_plot_lr <-
  scatter +
  geom_contour_filled(aes(x = x1,
                          y = x2,
                          z = pred_prob_logistic),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("#FFFFFF00", "#00000030")) +
  geom_contour(aes(x = x1,
                   y = x2,
                   z = pred_prob_logistic),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1) +
  xlim(c(-4, 4)) +
  ylim(-2.5, 12.5) +
  theme(legend.position = "none")

ill_plot_xgb <-
  scatter +
  geom_contour_filled(aes(x = x1,
                          y = x2,
                          z = pred_prob_xgb),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("#FFFFFF00", "#00000030")) +
  geom_contour(aes(x = x1,
                   y = x2,
                   z = pred_prob_xgb),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1) +
  xlim(c(-4, 4)) +
  ylim(-2.5, 12.5) +
  theme(legend.position = "none")

ggsave(here("for-paper",
            "revision_illustration_lr.pdf"),
       ill_plot_lr)
ggsave(here("for-paper",
            "revision_illustration_xgb.pdf"),
       ill_plot_xgb)


## --- Repeat but for a simple example ------
dat1 <-
  tibble(x1 = 5 * rnorm(n),
         x2 = .3 * x1 - .7 + .5 * rnorm(n),
         pop = "Population 1")
dat2 <-
  tibble(x1 = 5 * rnorm(n),
         x2 = .3 * x1 + .7 + .5 * rnorm(n),
         pop = "Population 2")

x <- bind_rows(dat1, dat2)


ggplot(x, aes(x = x1, y = x2, shape = pop)) +
  geom_point() +
  scale_shape_manual(values = c(16, 3)) +
  xlab("") +
  ylab("") +
  labs(shape = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")

# Build classification model using lr and xgb

# LR ----
ill_recipe <-
  recipe(pop ~ ., data = x)

# Specify the decistion tree
ill_mod <-
  logistic_reg() %>%
  set_mode("classification")

# Set up the workflow
ill_workflow <-
  workflow() %>%
  add_model(ill_mod) %>%
  add_recipe(ill_recipe)

fitted_lr <-
  ill_workflow %>%
  fit(data = x)

# XGB -----
ill_folds <- vfold_cv(x, v = 2)

xgb_mod <-
  boost_tree(
    trees = 1000,
    tree_depth = tune(), min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(), mtry = tune(),
    learn_rate = tune(),
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

# Set up the workflow
xgb_workflow <-
  workflow() %>%
  add_model(xgb_mod) %>%
  add_recipe(ill_recipe)

# Make a search grid for the parameters
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), x),
  learn_rate(),
  size = 30
)

# Calculate the cross-validated AUC for all the parameter combinations in the
# grid
xgb_tune_result <-
  tune_grid(
    xgb_workflow,
    resamples = ill_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )

# Which parameter combination is the best?
xgb_tune_result %>%
  select_best(metric = "roc_auc")

# Put the best parameters in the workflow
xgb_tuned <-
  finalize_workflow(
    xgb_workflow,
    parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_xgb <-
  xgb_tuned %>%
  fit(data = x)

# Make a plotting grid and predictions ----

cutoff <- .5
grid_resolution <- 100

newdata <-
  expand.grid(x1 = seq(from = -17,
                       to = 17,
                       length.out = grid_resolution),
              x2 = seq(from = -6,
                       to = 6,
                       length.out = grid_resolution))

predictions <-
  newdata %>%
  bind_cols(pred_prob_logistic = predict(fitted_lr,
                                         new_data = newdata,
                                         type = "prob")$`.pred_Population 1`) %>%
  bind_cols(pred_prob_xgb = predict(fitted_xgb,
                                    new_data = newdata,
                                    type = "prob")$`.pred_Population 1`)



# Make a plot -----
scatter <- 
  ggplot(x, aes(x = x1, y = x2, shape = pop)) +
  geom_point() +
  scale_shape_manual(values = c(16, 3)) +
  xlab("") +
  ylab("") +
  labs(shape = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ill_plot_lr <-
  scatter +
  geom_contour_filled(aes(x = x1,
                          y = x2,
                          z = pred_prob_logistic),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("#FFFFFF00", "#00000030")) +
  geom_contour(aes(x = x1,
                   y = x2,
                   z = pred_prob_logistic),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1) +
  theme(legend.position = "none")

ill_plot_xgb <-
  scatter +
  geom_contour_filled(aes(x = x1,
                          y = x2,
                          z = pred_prob_xgb),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("#FFFFFF00", "#00000030")) +
  geom_contour(aes(x = x1,
                   y = x2,
                   z = pred_prob_xgb),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1) +
  theme(legend.position = "none")

ggsave(here("for-paper",
            "revision_illustration_simple_lr.pdf"),
       ill_plot_lr)
ggsave(here("for-paper",
            "revision_illustration_simple_xgb.pdf"),
       ill_plot_xgb)
