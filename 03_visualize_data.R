# ==============================================================================
# Project: Tree Size and Maple Sap Production
# Script: 03_visualize_data.R
#
# Purpose:
# Visualize the relationship between tree diameter at breast height and maple
# sap volume, maple sap solubles content and maple syrup equivalent yield.
#
#
# Variables:
#   diameter_cm  = tree diameter at breast height (cm)
#   syrup_lb     = maple syrup equivalent yield (lbs)
#
# Input:
#   Data from multiple experiments and tests as imported by 0_read_data.R
#
# Output:
#   Scatter plots of maple sap volume, maple sap solubles content, and maple 
#   syrup equivalent yields versus tree diameter.
# ==============================================================================

# Import data -----
source("01_read_data.R")
source("02_analyse_data.R")

# Load dependencies ----
if(!existsFunction("%>%")) library(tidyverse)

# Scatter plot of maple syrup equivalent yield ----
ggplot(d, 
       aes(
         x = dbh, 
         y = syrup,
         colour = yr,
         shape = collection_system
         )
       ) +
  geom_line(
    data = plot_data,
    aes(x = dbh, y = Estimate, colour = collection_system, group = collection_system),
    inherit.aes = FALSE,
    linewidth = 1.2,
    color = "black",
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = plot_data,
    aes(
      x = dbh,
      ymin = Q2.5,
      ymax = Q97.5,
      fill = collection_system
    ),
    inherit.aes = FALSE,
    alpha = 0.2,
    colour = NA,
    show.legend = FALSE
  ) +
  geom_point(alpha = 0.8, size = 2.5) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) + 
  scale_color_manual(
    values = c(
      "2014" = "#1B9E77",
      "2015" = "#D95F02",
      "2016" = "#7570B3",
      "2017" = "#E7298A",
      "2018" = "#66A61E",
      "2019" = "#E6AB02",
      "2020" = "#A6761D",
      "2021" = "#666666",
      "2022" = "#1F78B4",
      "2023" = "#B2DF8A",
      "2024" = "#FB9A99",
      "2025" = "#A6CEE3"
    )
  ) +
  scale_shape_manual(
    values = c(
      vac = 16,
      gra = 15
    )
  ) +
  labs(
    x = "Tree diameter at breast height (cm)",
    y = "Syrup equivalent yield (lbs)",
    colour = "Year",
    shape = "Collection System",
    title = "Relationship Between Tree Diameter and Syrup Yield"
  ) +
  theme_bw()
