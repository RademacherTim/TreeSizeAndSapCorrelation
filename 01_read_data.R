# ==============================================================================
# Project: Tree Size and Maple Sap Production
# Script: 01_read_data.R
#
# This script imports data from multiple experiments and tests to build a global 
# equation linking trees diameter, as a proxy for tree size, to syrup yield. 
# 
# Experiments and tests:
#         - 2016 data collected on individual sugar maple trees at the Proctor 
#           Maple Research Center using chambers attached to trees and 
#           maintained under approximately 25" Hg vacuum throughout the sap 
#           season (Isselhardt et al., 2018)
#
# Variables:
#   dbh_in         = tree diameter (inches) -> converted to cm
#   syrup_lb       = maple syrup equivalent yield (lbs)
# ==============================================================================

# Load dependencies ----
library(tidyverse)
library(readxl)

# Read data from Isselhardt et al. (2018) for 2016 and 2017 ----
# TR - I need to eventually add treeIDs to this data to capture individual trees effects appropriately
syrup_2016 <- read_csv(
  "./data/2016_data.csv",
  col_names = c("dbh_in", "syrup")
) |> 
mutate(
  yr = 2016,
  collection_system = factor("vac"),
  vac = 25,
  dbh = dbh_in * 2.54,
  tree_id = factor(NA)
) |>
select (-dbh_in)

syrup_2017 <- read_csv(
  "./data/2017_data.csv",
  col_names = c("dbh_in", "syrup")
) |> 
mutate(
  yr = 2017,
  collection_system = factor("vac"),
  vac = 25,
  dbh = dbh_in * 2.54,
  tree_id = factor(NA)
) |> 
select (-dbh_in)
  
# Combine tibble from 2016 and 2017 ----  
syrup <- rbind(syrup_2016, syrup_2017)
rm(syrup_2016, syrup_2017)

# Rearrange the order and add missing values for sap soluble content ----
syrup <- syrup %>% 
  mutate(ssc = as.numeric(NA)) %>% 
  select(tree_id, collection_system, yr, syrup, ssc, dbh, vac)

# ============================================================
# Read Long-term study data from Proctor
# ============================================================
lts_raw <- read_excel(
  "../PMRC long-term tree health study/Data/LTS-Sugar_yield.xlsx",
  sheet = "Tree Growth and Yield",
  col_names = TRUE,
  col_types = c(
    "numeric",
    rep("text", 11), 
    rep("numeric", 19), # DBH x 13 & growth variables
    "logical",          # Empty 
    "text",             # Notes
    rep("numeric", 2),  #
    "text",
    rep("numeric", 2),
    "text",
    rep("numeric", 7),
    rep("text", 2),
    rep("numeric", 2),
    "text",
    rep("numeric", 2),
    "logical" ,         # Empty
    rep("text", 12),    # Taphole closure  
    "logical",          # Empty
    rep("numeric", 13), # Sap sugar content
    "logical",          # Legacy calculation
    "text",             # Treatment
    rep("numeric", 16), # Syrup equivalent yield
    "logical",          # Empty
    rep("text", 6),     # Notes
    rep("logical", 3)   # Empty, lat and lon
  ),
  n_max = 92
)

# DBH columns (2013-2025) ----
dbh_cols <- names(lts_raw)[13:25]    # M:Y

# SSC columns (2014-2026) ----
ssc_cols <- names(lts_raw)[68:80]    # BP:CB

# Syrup columns (2014-2026) ----
syrup_cols <- names(lts_raw)[84:96]  # CF:CR

# ------------------------------------------------------------------
# Create long DBH table
# ------------------------------------------------------------------
dbh_long <- lts_raw %>%
  select(
    tree_id = 4,           # column D
    collection_system = 2, # column B
    all_of(dbh_cols)       # M:Y
  ) %>%
  pivot_longer(
    cols = all_of(dbh_cols),
    names_to = "dbh_year",
    values_to = "dbh_in"
  ) %>% 
  filter ( # Only look at data from the gravity and vacuum treatments
    collection_system == "Gravity" | collection_system == "Vacuum"
  ) %>%
  mutate( # Homogenize designation of collection system as factor
    collection_system = ifelse(collection_system == "Gravity", "gra", "vac") |>
      factor()
  ) %>%
  mutate( # Extract correct year N.B. DBH is measured at the end of the year and 
    # corresponds to the following sugaring season
    yr = ifelse(dbh_year == "Inital dbh 2013 (in)", 
                2013, 
                str_sub(dbh_year, 1, 4)) |> as.numeric() + 1
  ) %>%
  mutate( # Convert inches to cm
    dbh = dbh_in * 2.54
  ) %>% 
  mutate( # Convert tree ID to factor
    tree_id = factor(tree_id)
  ) %>%
  select(tree_id, collection_system, yr, dbh)

# ------------------------------------------------------------------
# Create long sap soluble content table
# ------------------------------------------------------------------
ssc_long <- lts_raw %>%
  select(
    tree_id = 4,
    collection_system = 2,
    all_of(ssc_cols)
  ) %>%
  pivot_longer(
    cols = all_of(ssc_cols),
    names_to = "yr",
    values_to = "ssc"
  ) %>% 
  filter ( # Only look at data from the gravity and vacuum treatments 
    collection_system == "Gravity" | collection_system == "Vacuum"
  ) %>%
  mutate( # Homogenize designation of collection system as factor 
    collection_system = ifelse(collection_system == "Gravity", "gra", "vac") |>
      factor()
  ) %>%
  mutate( # Convert yr to factor
    yr = str_sub(yr, 1, 4) |> as.numeric()
  ) %>% 
  mutate( # Convert tree ID to factor
    tree_id = factor(tree_id)
  ) %>%
  select(tree_id, collection_system, yr, ssc)

# ------------------------------------------------------------------
# Create long syrup table
# ------------------------------------------------------------------
syrup_long <- lts_raw %>%
  select(
    tree_id = 4,
    collection_system = 2,
    all_of(syrup_cols)
  ) %>% 
  mutate(
    `2025 Syrup Yield (gpt)` = as.numeric(`2025 Syrup Yield (gpt)`)
  ) %>% 
  pivot_longer(
    cols = all_of(syrup_cols),
    names_to = "yr",
    values_to = "syrup"
  ) %>% 
  filter ( # Only look at data from the gravity and vacuum treatments
    collection_system == "Gravity" | collection_system == "Vacuum"
  ) %>%
  mutate( # Homogenize designation of collection system as factor 
    collection_system = ifelse(collection_system == "Gravity", "gra", "vac") |>
      factor()
  ) %>%
  mutate( # Convert yr to factor
    yr = str_sub(yr, 1, 4) |> as.numeric()
  ) %>% 
  mutate( # Convert tree ID to factor
    tree_id = factor(tree_id)
  )  %>%
  select(tree_id, collection_system, yr, syrup)

# ------------------------------------------------------------------
# Join SSC and syrup data
# ------------------------------------------------------------------

lts <- syrup_long %>%
  left_join(ssc_long, by = c("tree_id", "collection_system", "yr")) %>% 
  left_join(dbh_long, by = c("tree_id", "collection_system", "yr"))

# Convert gallons to pounds ----
lts <- lts %>% mutate(syrup = syrup * 11) # 11 lbs in a gallon

# Add missing vacuum level data, i.e. it is unknown ----
lts <- lts %>% mutate(vac = as.numeric(NA))

# Combine PMRC long-term data with previously added data ----
d <- rbind(lts, syrup)

# Clean workspace ----
rm(lts, lts_raw, syrup, dbh_long, ssc_long, syrup_long, dbh_cols, ssc_cols, 
   syrup_cols)

# Convert year to a factor for modelling ----
d <- d %>% mutate(yr = factor(yr))

# Read acer-web data ----

# Read Acer-Net data ----

#===============================================================================