## Set environment -------------------------------------------------------------
library(tidyverse)
library(data.table)
library(furrr)
# library(purrr)
options(future.globals.maxSize= 89128960000)
plan(multiprocess, workers = 15)


filename = "./data/data_for_bayesian_synthetic_v2.Rds"

## Read data -------------------------------------------------------------------
data = readRDS(filename)

## Run -------------------------------------------------------------------------
library(ggpubr)
library(ggrepel)

is_outlier <- function(x, n) {
  return(x < quantile(x, 0.25) - n * IQR(x) | x > quantile(x, 0.75) + n * IQR(x))
}

boxplot_change_rate = function(df, n, target, title){
  df = df[c(target,"Date","County", "synthetic")] %>% `colnames<-`(c("value","time","county", "synthetic"))
  df = df %>% 
    group_by(time) %>%
    mutate(outlier = ifelse(is_outlier(value, n), 1, as.numeric(NA)))
  df = df %>%
    mutate(county_outlier = case_when(as.numeric(outlier) == 1 ~ as.character(county), # county
                                      TRUE ~ NA_character_))
  bplot = ggplot(df, aes(x = time, y=value * 100, color = synthetic)) + 
    geom_boxplot() + 
    geom_text_repel(aes(label = county_outlier), na.rm = TRUE, hjust = - 0.2) + 
    scale_color_manual(values=c("grey30", "red4")) +
    ylab("Percentage") + 
    xlab(NULL) + 
    # ylim(0,0.052) +
    ggtitle(paste0(title)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  return(bplot)
}

figures = future_map2(
  data[71:84],
  names(data[71:84]) %>% as.list,
  ~{
    df = .x
    name = .y
    var = strsplit(name, "-")[[1]][1]
    figure = boxplot_change_rate(df, 3, var, name)
    figure
  }
)

i = 0

i = i + 1
print(i)
target = figures
file_name = names(target)[1] %>% 
  strsplit(., "-")
file_name = file_name[[1]][1:2] %>% 
  paste0(., collapse = "_") %>% 
  paste0("./figures/synthetic/bplot_", ., "_percent.pdf")

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


