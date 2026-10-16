# ==============================================================================
# Project: Tree Size and Maple Sap Production
# Script: 02_analyse_data.R
#
# Purpose:
# Fit Bayesian mixed-effects models relating maple syrup equivalent yield to 
# tree diameter (cm).
#
# Models:
# 1. Linear effect of DBH
# 2. Asymptotic (plateau) effect of DBH
#
# Random Effects:
# - Year
# - Tree ID (TR - Needs to be added to Mark's data)
# - TR Need to eventually add "vac" versus "gra" as a linear effect, based on 
# the prior knowledge by Tim Wilmot's work
#
# Response:
# syrup
#
# Predictor:
# dbh
#
# Model Comparison:
# Leave-one-out cross validation (LOO)
# ==============================================================================

# Import data -----
source("01_read_data.R")

# Load dependencies ----
if(!existsFunction("%>%")) library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)

# Linear model ----
mod_linear <- brm(
  syrup | trunc (lb = 0) ~
    dbh * collection_system +
    (1 | yr), #+
    # (1 | tree_id), # TR - I will have to eventually add tree_id, when I got them for Mark's data
  data = d %>% select (-vac, -tree_id),
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 6000,
  seed = 123
)
summary(mod_linear)
plot(mod_linear)
fixef(mod_linear) # TR - The intercept is negative but cannot be. I need to see whether I can force this!

plot(
  conditional_effects(mod_linear),
  points = TRUE
)

# Generate population-level predictions to plot the conditional effect ----
newdata <- expand_grid(
  dbh = seq(
    min(d$dbh),
    max(d$dbh),
    length.out = 200
  ),
  collection_system = c("gra", "vac")
)

pred <- fitted(
  mod_linear,
  newdata = newdata,
  re_formula = NA
)

plot_data <- bind_cols(
  newdata,
  as.data.frame(pred)
)
