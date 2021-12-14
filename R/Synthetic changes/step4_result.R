## Set Environment -------------------------------------------------------------
library(tidyverse)
library(furrr)
library(ggforce)
options(future.globals.maxSize= 89128960000)
plan(multiprocess, workers = 10)

## Load Data -------------------------------------------------------------------
data_multiply = readRDS("./data_for_bayesian_synthetic_v2.Rds")
data_add = readRDS("./data_for_bayesian_synthetic.Rds")
t_score_add = readRDS("./t_score_add.Rds")
t_score_multiply = readRDS("./t_score_multiply.Rds")


## Find t score ----------------------------------------------------------------
for (i in 1:length(data_add)) {
  data_add[[i]]$t_score = NA_real_
  rows = which(data_add[[i]]$synthetic)
  for (row in rows) {
    var = paste0("Q2.5.",data_add[[i]]$var[row])
    data_add[[i]]$t_score[row] = t_score_add[[i]][row,var]
  }
}

for (i in 1:length(data_multiply[71:84])) {
  data_multiply[[i+70]]$t_score = NA_real_
  rows = which(data_multiply[[i+70]]$synthetic)
  for (row in rows) {
    var = paste0("Q2.5.",data_multiply[[i+70]]$var[row])
    data_multiply[[i+70]]$t_score[row] = t_score_multiply[[i]][row,var]
  }
}


## Compute IQR -----------------------------------------------------------------
compute_IQR <- function(x) {
  upper = (x - quantile(x, 0.75))/IQR(x)
  lower = (quantile(x, 0.25) - x)/IQR(x)
  res = case_when(upper > 0 ~ upper,
                  lower > 0 ~ lower,
                  TRUE ~ 0)
  return(res)
}

get_IQR = function(data){
  for (i in 1:length(data)) {
    target = data[[i]] %>% filter(!is.na(var)) %>% .[1, "var"]
    data[[i]]$target = data[[i]][[target]]
    data[[i]] = data[[i]] %>% 
      group_by(County) %>% 
      mutate(
        county_IQR = compute_IQR(target)
      ) %>% 
      ungroup %>% 
      group_by(Date) %>% 
      mutate(
        date_IQR = compute_IQR(target)
      ) %>% 
      ungroup
  }
  
  res = data %>% 
    do.call("rbind",.) %>% 
    filter(synthetic) %>% 
    select(c("Date","County","origin","new","level","var",
             "county_IQR","date_IQR","t_score"))
  
  return(res)
}

result = rbind(get_IQR(data_add) %>% mutate(operation = "Addition"),
               get_IQR(data_multiply[71:84]) %>% mutate(operation = "Multiplication"))


## scatter plot ----------------------------------------------------------------
result = result %>% 
  mutate(
    Outlier = ifelse(t_score > 1.96, TRUE, FALSE),
    log_county_IQR = log(county_IQR + 0.000001),
    log_date_IQR = log(date_IQR + 0.000001),
    IQR_outlier = ifelse(county_IQR>=1.5 | date_IQR>=1.5, TRUE, FALSE)
  )

fig = result %>% 
  ggplot(aes(x = county_IQR, y = date_IQR)) +
  geom_point(aes(color=IQR_outlier, shape=Outlier), size = 3) +
  geom_hline(yintercept = 1.5) + 
  geom_vline(xintercept = 1.5) +
  annotate("text", x=9.5, y=1.7, 
           label="1.5×IQR", size=3) +
  annotate("text", x=1.7, y=9.5, 
           label="1.5×IQR", size=3, angle = -90) +
  geom_ellipse(aes(x0 = 0, y0 = 1.85, a = 0.4,
                   b = 0.4, angle = 0),colour = "brown2") +
  geom_ellipse(aes(x0 = 0.4, y0 = 0.69, a = 0.3,
                   b = 0.3, angle = 0),colour = "brown2") +
  xlim(-0.5,10) +
  ylim(-0.5,10) +
  scale_shape_manual(values=c(1, 17)) +
  scale_color_manual(values=c("grey50", "#005CAB")) +
  guides(color=guide_legend(override.aes=list(shape=15,size=5))) +
  theme_bw() +
  xlab("IQR Score by County") +
  ylab("IQR Score by Date") +
  labs(color = "IQR Detected\nOutlier",
       shape = "Model Predicted\nDeviation (α<0.05)")

pdf("IQR_vs_dev.pdf", width=8, height=6.5)
fig
dev.off() 


