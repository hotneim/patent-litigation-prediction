# -- TIDYMODELS FOR LITIGATION PROJECT --------------------------------------- #
# -- ROBUSTNESS CHECKS                 --------------------------------------- #
#
# We create the necessary figures for robustness checks.

library(tidyverse)
library(here)
library(scales) # For consistent coloring 

# Read the variable names
variables <- 
  read_csv(here("data", "variable_names.csv")) %>% 
  rename(variable = "old")

# Collect the AUC
auc <-
  tibble(files = dir(here("est-results"), pattern = "robustness*", full.names = TRUE)) %>% 
  mutate(file = files) %>%
  separate(files, into = c("bla1", "year", "bla2"), sep = "_", ) %>%
  select(-bla1) %>%
  mutate(bla2 = gsub("block1-2-3-4-5-results.", "", .$bla2)) %>%
  separate(bla2, into = c("model", "bla3")) %>%
  select(year, model, file) %>%
  mutate(auc_test = NA, auc_train = NA)

vi <- list()

# Read the results
for(i in seq_len(nrow(auc))) {
  load(auc$file[i])
  auc$auc_train[i] <- result$auc$value[result$auc$where == "test"]
  auc$auc_test[i] <- result$auc$value[result$auc$where == "train"]

  vi[[i]] <-
    result$vi_perm %>%
    filter(where == "test") %>%
    group_by(variable) %>%
    summarise(vi = (auc$auc_test[i] - mean(perm_auc)) / auc$auc_test[i])
}

# Extract the AUC
auc <-
  auc %>%
  mutate(vi = vi) %>%
  mutate(year = as.numeric(year) - 1) %>% 
  filter(year <= 2011)

# Make plot of the AUC for xgb over time
auc_plot <-
  auc %>%
  filter(model == "xgb") %>% 
  ggplot(aes(x = year, y = auc_test)) +
  geom_line() +
  xlab ("") +
  ylab("") +
  theme_minimal() +
  scale_x_continuous(breaks = auc$year %>% unique())
  
ggsave(filename = here("for-paper", "robustness_auc_xgb.pdf"),
       plot = auc_plot, width = 6, height = 4)

# Plot of the variable importance of xgb over time, highlighting the top three 
# variables.

# We want to highlight the top three variables
top3 <- c("Assignee experience", "Family size", "NBER tech fields")

# Consistent coloring
my_palette <- set_names(hue_pal()(3), top3)

vi_plot_data <- 
  auc %>%
  unnest(vi) %>%
  filter(model == "xgb") %>% 
  full_join(variables) %>% 
  select(-variable) %>% 
  mutate(Variable = new) %>% 
  mutate(Variable = case_when(!(Variable %in% top3) ~ "Other",
                              .default = Variable))
vi_plot <-
  vi_plot_data %>% 
  filter(Variable == "Other") %>% 
  ggplot(aes(x = year, y = vi, group_by = new)) +
  geom_line(colour = "darkgray", size = .5) +
  geom_line(aes(x = year, y = vi, colour = Variable),
            size = 1.1,
            data = vi_plot_data %>% filter(Variable != "Other")) +
  xlab ("") +
  ylab("") +
  theme_minimal() +
  scale_x_continuous(breaks = auc$year %>% unique()) +
  scale_colour_manual(values = my_palette) +
  labs(colour = "") +
  theme(legend.position = "bottom") 

ggsave(filename = here("for-paper", "robustness_vi_xgb.pdf"),
       plot = vi_plot, width = 6.5, height = 4.5)

# Function for making the plot of ranks over time with highligts
make_rankplot <- function(model_type, auc, vi) {
  
  ranks <-
    auc %>%
    mutate(vi = vi) %>%
    mutate(year = as.numeric(year)) %>%
    unnest(vi) %>%
    group_by(year, model) %>%
    mutate(rank = rank(-vi)) %>%
    ungroup()
  
  ranks_highlight <-
    ranks %>%
    filter(model == model_type) %>%
    filter((year %in% c(2006)), rank <= 3)
  
  highlighted_vars <-
    ranks %>%
    left_join(variables) %>% 
    filter(model == model_type) %>%
    filter(new %in% top3) 
    
  
  rank_plot <-
    ranks %>%
    filter(model == model_type) %>%
    filter(!(variable %in% (ranks_highlight %>%
                              filter(model == model_type) %>%
                              select(variable) %>%
                              pull))) %>%
    ggplot(aes(x = year, y = rank, group_by = variable)) +
    geom_line(color = "#00000015") +
    geom_line(aes(colour = new),
              data = highlighted_vars,
              linewidth = 1.1) +
    scale_colour_manual(values = my_palette) +
    theme_minimal() +
    scale_x_continuous(breaks = ranks$year %>% unique, position = "top") +
    scale_y_reverse(breaks = ranks$rank %>% unique) +
    xlab("") + ylab("") +
    labs(colour = "") +
    theme(legend.position = "bottom")
  
  rank_plot
}

rank_plot <- make_rankplot(model_type = "xgb", auc = auc, vi = vi)

ggsave(filename = here("for-paper", "robustness_rank_plot.pdf"),
       plot = rank_plot, width = 6.5, height = 6)

