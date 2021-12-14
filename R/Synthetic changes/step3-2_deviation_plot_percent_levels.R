## Environment --------------------------------------------------------------------------------------
list.of.packages <- c("tidyverse", "magrittr", "furrr", "brms")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(magrittr)
library(furrr)
library(brms)
library(rstan)
# library(Amelia)
library(ggplot2)
library(ggrepel)
library(ggpubr)

options(digits = 5, scipen = 999)
plan(multiprocess, workers = 15)
rstan_options(auto_write = TRUE)

## Load data ---------------------------------------------------------------------------------------
fit_res = readRDS("./data/fit_brms_result_synthetic_v2.Rds")
data = readRDS("./data/data_for_bayesian_synthetic_v2.Rds")[71:84]


## Predict -----------------------------------------------------------------------------------------
col_names = c("Dropped","Added","LastName","Address","VbmVoterType","PartyCode","DOB")

scatter_deviation = function(t_score, df_proportion, title){
  deviation = data.frame(score = unlist(t_score),
                         time = df_proportion$Date,
                         county = df_proportion$County,
                         synthetic = df_proportion$synthetic)
  deviation["county"][deviation["score"]<1.96] = NA
  splot = ggplot(deviation, aes(x = time, y=score, color=synthetic)) + 
    geom_point() + 
    geom_text_repel(aes(label = county), na.rm = TRUE, hjust = - 0.2) + 
    ylab("t score") + 
    xlab(NULL) + 
    geom_hline(yintercept=1.96, linetype="dashed") +
    ggtitle(paste0(title)) +
    scale_color_manual(values=c("grey30", "red4")) +
    theme_bw() +
    # ylim(0, 20) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  return(splot)
}

compute_t_p_value = function(x, loc_est, loc_se, n){
  result = x[,loc_est]/x[,loc_se]
  result = cbind(result,pt(abs(unlist(result)),df = n, lower.tail = FALSE)*2)
  colnames(result) = c("t value", "Pr(>|t|)")
  return(result)
}


figures = NULL
t_score_list = NULL
for (i in 1:length(data)) {
  name = names(data)[i]
  print(name)
  fit_0 = fit_res[[name]]
  df_proportion = data[[name]]
  var = strsplit(name, "-")[[1]][1]
  var_ind = which(col_names == var)
  
  predicted = data.frame(predict(fit_0))
  
  predicted = predicted %>% 
    mutate(
      Estimate.Dropped = unlist(abs(Estimate.Dropped - df_proportion["Dropped"])),
      Estimate.Added = unlist(abs(Estimate.Added - df_proportion["Added"])),
      Estimate.LastName = unlist(abs(Estimate.LastName - df_proportion["LastName"])),
      Estimate.Address = unlist(abs(Estimate.Address - df_proportion["Address"])),
      Estimate.VbmVoterType = unlist(abs(Estimate.VbmVoterType - df_proportion["VbmVoterType"])),
      Estimate.PartyCode = unlist(abs(Estimate.PartyCode - df_proportion["PartyCode"])),
      Estimate.DOB = unlist(abs(Estimate.DOB - df_proportion["DOB"]))
    )
  
  for (i in 1:7) {
    temp = predicted[((i-1)*4+1):((i-1)*4+4)]
    temp[3:4] = round(compute_t_p_value(temp[1:2],1,2,10000),6)
    predicted[((i-1)*4+1):((i-1)*4+4)] = temp
  }
  
  t_score = predicted[(1:7)*4-1]
  
  figures[[name]] = scatter_deviation(t_score[, var_ind], df_proportion, name)
  t_score_list[[name]] = t_score
}

# save files
saveRDS(t_score_list, "./data/t_score_multiply.Rds")


i = 0

i = i + 1
print(i)
target = figures
file_name = names(target)[1] %>% 
  strsplit(., "-")
file_name = file_name[[1]][1:2] %>% 
  paste0(., collapse = "_") %>% 
  paste0("./figures/synthetic/dev_", ., "_percent.pdf")

pdf(file_name, width=20, height=70)
ggarrange(target[[1]], target[[2]], target[[3]],
          target[[4]], target[[5]], target[[6]],
          target[[7]], target[[8]], target[[9]],
          target[[10]], target[[11]], target[[12]],
          target[[13]], target[[14]],
          ncol = 2, nrow = 7,
          labels = c("A", "B", "C", "D", "E", "F", "G",
                     "H", "I", "J", "K", "L", "M", "N"), label.x = 0.01)
dev.off()

