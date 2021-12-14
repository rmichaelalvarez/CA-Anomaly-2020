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
fit_0 = readRDS("./data/fit_brms_zero_inflated_beta_logit_identity_identity.Rds")
df_proportion = readRDS("./data/data_for_bayesian.Rds")


## Predict -----------------------------------------------------------------------------------------
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

compute_t_p_value = function(x, loc_est, loc_se, n){
  result = x[,loc_est]/x[,loc_se]
  result = cbind(result,pt(abs(unlist(result)),df = n, lower.tail = FALSE)*2)
  colnames(result) = c("t value", "Pr(>|t|)")
  return(result)
}


for (i in 1:7) {
  temp = predicted[((i-1)*4+1):((i-1)*4+4)]
  temp[3:4] = round(compute_t_p_value(temp[1:2],1,2,10000),6)
  predicted[((i-1)*4+1):((i-1)*4+4)] = temp
}

t_score = predicted[(1:7)*4-1]

# number of deviations
num_dev = aggregate(data.frame(n = rowSums(t_score >= 1.96)), by=list(county=df_proportion$County), FUN=sum)
num_dev = num_dev[order(num_dev$n,decreasing=T),]
num_dev$county = factor(num_dev$county,levels=rev(as.character(num_dev$county)))
plot_num_dev = ggplot(num_dev %>% filter(n>0), aes(x=county,y=n)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_bw() +
  ylab("Number of Deviations") +
  xlab("County") +
  theme(panel.border = element_blank(), axis.ticks=element_blank()) +
  ggtitle("How Many Times Does a County Deviate from the Model Prediction") +
  coord_flip()

saveRDS(num_dev, "./data/num_dev.Rds")

pdf("./figures/original/dev_summary.pdf", width=8.5, height=11)
plot_num_dev
dev.off()

# t score scatter plots
titles = c("Dropped","Added","Last Name Changed","Address Changed","VBM Status Changed","Party Changed","Birth Date Changed")


scatter_deviation = function(t_score, df_proportion, titles, i, thres = 1.96){
  deviation = data.frame(score = unlist(t_score[,i]),time = df_proportion$Date,county = df_proportion$County)
  deviation["county"][deviation["score"]<thres] = NA
  splot = ggplot(deviation, aes(x = time, y=score)) + 
    geom_point() + 
    geom_text_repel(aes(label = county), na.rm = TRUE, hjust = - 0.2) + 
    ylab("t score") + 
    xlab(NULL) + 
    geom_hline(yintercept=thres, linetype="dashed") +
    ggtitle(paste0(titles[i], " Rates")) +
    theme_bw() +
    # ylim(0, 20) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  return(splot)
}

splot_dropped = scatter_deviation(t_score, df_proportion, titles, 1, 2.58)
splot_added = scatter_deviation(t_score, df_proportion, titles, 2, 2.58)
splot_name_last = scatter_deviation(t_score, df_proportion, titles, 3, 2.58)
splot_addr = scatter_deviation(t_score, df_proportion, titles, 4, 2.58)
splot_voter_status = scatter_deviation(t_score, df_proportion, titles, 5, 2.58)
splot_party = scatter_deviation(t_score, df_proportion, titles, 6, 2.58)
splot_birth_date = scatter_deviation(t_score, df_proportion, titles, 7, 2.58)

pdf("./figures/original/dev_dropped.pdf", width=12, height=12)
splot_dropped
dev.off()

pdf("./figures/original/dev_added.pdf", width=12, height=12)
splot_added
dev.off()

pdf("./figures/original/dev_name_last.pdf", width=12, height=12)
splot_name_last
dev.off()

pdf("./figures/original/dev_res_addr.pdf", width=12, height=12)
splot_addr
dev.off()

pdf("./figures/original/dev_vbm_type.pdf", width=12, height=12)
splot_voter_status
dev.off()

pdf("./figures/original/dev_party.pdf", width=12, height=12)
splot_party
dev.off()

pdf("./figures/original/dev_birth_date.pdf", width=12, height=12)
splot_birth_date
dev.off()

# pdf("dev_added_dropped.pdf", width=5, height=10)
# ggarrange(splot_added, splot_dropped, ncol = 1, nrow = 2, labels = c("A", "B"), label.x = 0.02)
# dev.off()
# 
# pdf("dev_name_addr_status.pdf", width=5, height=15)
# ggarrange(splot_name_last, splot_addr, splot_voter_status,
#           ncol = 1, nrow = 3,
#           labels = c("A", "B", "C"), label.x = 0.01)
# dev.off()
# 
# pdf("dev_party_dob.pdf", width=5, height=10)
# ggarrange(splot_party, splot_birth_date, ncol = 1, nrow = 2,
#           labels = c("A", "B"), label.x = 0.01)
# dev.off()

# save t_test
t_test = cbind(df_proportion[c("Date","County","VoterCount")],
               t_score)

colnames(t_test)[4:10] = str_remove(colnames(t_score),"Q2.5.")
t_test = reshape2::melt(t_test,
                        id.vars = c("Date","County","VoterCount"))
colnames(t_test)[4:5] = c("Variable","t_score")
t_test$is_outlier = t_test$t_score >= 1.96

saveRDS(t_test, "./data/t_test.Rds")