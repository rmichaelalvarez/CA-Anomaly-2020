## Set Environment -------------------------------------------------------------
library(tidyverse)
library(furrr)
options(future.globals.maxSize= 89128960000)
plan(multiprocess, workers = 30)


## Read Data -------------------------------------------------------------------
data = readRDS("./data/data_for_bayesian.Rds")


## Specify Rules ---------------------------------------------------------------
levels = c(1.25, 1.5, 2, 4, 8, 16, 32)
methods = c("scattered", "together")
var_list = c("Dropped", "Added", "LastName",
              "Address", "VbmVoterType",
              "PartyCode", "DOB")
counties = c("Trinity", "Kings", "Los Angeles",
             "Colusa", "Butte",
             "San Francisco", "Nevada")

## Generate Data ---------------------------------------------------------------
data_syn = NULL
set.seed(215)
for (i in 1:length(var_list)) {
  var = var_list[i]
  county = counties[i]
  for (level in levels) {
    for (method in methods) {
      temp = data
      temp = temp %>% 
        mutate(
          origin = NA_real_,
          new = NA_real_,
          level = NA_real_,
          var = NA_character_,
          synthetic = FALSE
        )
      ind = which(temp$County == county)
      if (method == "scattered") {
        select_ind = sample(ind, 4)
      }else{
        start = sample(1:(length(ind) - 3), 1)
        select_ind = ind[start:(start + 3)]
      }
      temp$origin[select_ind] = temp[select_ind, var]
      temp[select_ind, var] = temp[select_ind, var] * level
      temp$new[select_ind] = temp[select_ind, var]
      temp$level[select_ind] = level
      temp$var[select_ind] = var
      temp$synthetic[select_ind] = TRUE
      data_syn[[paste(var, county, level, method, sep = "-")]] = temp
    }
  }
}

saveRDS(data_syn, "./data/data_for_bayesian_synthetic_v2.Rds")



