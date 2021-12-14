list.of.packages <- c("tidyverse", "magrittr", "furrr", "brms")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
# library(magrittr)
# library(furrr)
library(brms)
library(rstan)
# library(Amelia)
# library(ggplot2)
# library(ggrepel)
# library(ggpubr)

options(digits = 5, scipen = 999)
# plan(multicore, workers = availableCores() - 2)
rstan_options(auto_write = TRUE)

## Load data -------------------------------------------------------------------
data = readRDS("./data/data_for_bayesian.Rds")

## Prepare data ----------------------------------------------------------------

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

model = as.formula(paste0("mvbind(", paste0(dependent_var,
                                            collapse = (", ")), ") ~",
                          paste0(c(group_terms),collapse = (" + "))))

## Estimation ------------------------------------------------------------------
fit_brms <- brm(model, data = data, iter = 12000, cores = 5, chains = 4,
             family = family, control = list(max_treedepth = 20,
                                             adapt_delta = 0.99))

summary(fit_brms)
saveRDS(fit_brms, file = paste0("./data/fit_brms_",family$family,"_",
                                family$link,"_",family$link_phi,
                                "_",family$link_zi,".Rds"))

