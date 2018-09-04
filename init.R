### Libraries
library(ggpubr)
library(tidyverse)
library(lubridate)
library(stringr)
library(splines)
library(dslabs)
library(readxl)
library(purrr)
library(broom)

### Setting default theme for all plots
ds_theme_set(new="theme_minimal")

### Loading functions
source("functions.R")

### Global variables 
last_day <- as.Date("2018-05-31")
nknots   <- 4.5

hurricane_dates <- list(FL = as.Date(c("2017-09-10")),
                        LA = as.Date(c("2005-08-29")),
                        NJ = as.Date(c("2012-10-29")),
                        PR = as.Date(c("1989-09-18","1998-09-21","2017-09-20")))

hurricane_names <-  list(FL = c("FL: Irma"),
                         LA = c("LA: Katrina"),
                         NJ = c("NJ: Sandy"),
                         PR = c("PR: Hugo", "PR: Georges", "PR: Maria"))