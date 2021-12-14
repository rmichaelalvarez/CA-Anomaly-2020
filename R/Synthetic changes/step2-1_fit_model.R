## Set Environment -------------------------------------------------------------
library(tidyverse)
library(furrr)
library(brms)
library(rstan)
options(future.globals.maxSize= 89128960000)
plan(multisession, workers = 30)


data_path = "data/"


## Read Data -------------------------------------------------------------------
data = readRDS("./data/data_for_bayesian_synthetic.Rds")


## Bayesian model --------------------------------------------------------------
family = zero_inflated_beta(link = "logit",
                            link_phi = "identity",
                            link_zi = "identity")

# Specify the model
population_terms = c("")
group_terms      = c("(1|p|Date)", "(1|q|County)")
dependent_var    = c("Dropped", "Added", "LastName",
                     "Address", "VbmVoterType",
                     "PartyCode", "DOB")

model = as.formula(paste0("mvbind(",
                          paste0(dependent_var, collapse = (", ")),
                          ") ~",
                          paste0(c(group_terms),collapse = (" + "))))

## Estimation ------------------------------------------------------------------
result = NULL
for (i in 1:(length(levels)*length(methods))) {
  print(paste0("Iteration--", i))
  target = data_syn[(0:(length(var_list) - 1))*(length(levels)*length(methods)) + i]
  output = target %>% 
    future_map(
      ~{
        data = .
        fit_brm = brm(model,
                      data = data,
                      iter = 12000,
                      cores = 4,
                      chains = 4,
                      family = family,
                      control = list(max_treedepth = 20, adapt_delta = 0.99))
        fit_brm
      }
    )
  result = c(result, output)
}

## Finish ----------------------------------------------------------------------
# saveRDS(result, "./data/bayesian/fit_brms_result_synthetic.Rds")

future_map2(
  result,
  names(result),
  ~{
    data = .x
    name = .y
    saveRDS(data, paste0("./data/bayesian/fit_brms_",
                         name, ".Rds"))
  }
)


