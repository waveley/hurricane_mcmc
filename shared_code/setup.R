####################
#
# Setup
#
####################

set.seed(20220417)
library(tidyverse)
library(lubridate)
library(extraDistr)
library(MASS)


# set knitr defaults
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.width = 6,
  fig.asp   = .6,
  out.width = "90%",
  cache = TRUE
)

# set theme defaults
theme_set(
  theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title    = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption  = element_text(hjust = 0.0)
    )
)

# set color scale defaults
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill   = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete   = scale_fill_viridis_d