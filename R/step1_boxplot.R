## Set environment -------------------------------------------------------------
library(tidyverse)
library(data.table)
library(furrr)
# library(purrr)
options(future.globals.maxSize= 89128960000)
plan(multiprocess, workers = 30)


filename = "./data/data_for_bayesian.Rds"

## Read data -------------------------------------------------------------------
data = readRDS(filename)

## Run -------------------------------------------------------------------------
library(ggpubr)
library(ggrepel)

is_outlier <- function(x, n) {
  return(x < quantile(x, 0.25) - n * IQR(x) | x > quantile(x, 0.75) + n * IQR(x))
}

boxplot_change_rate = function(df, n, target, title){
  df = df[c(target,"Date","County")] %>% `colnames<-`(c("value","time","county"))
  df = df %>% 
    group_by(time) %>%
    mutate(outlier = ifelse(is_outlier(value, n), 1, as.numeric(NA)))
  df = df %>%
    mutate(county_outlier = case_when(as.numeric(outlier) == 1 ~ as.character(county), # county
                                      TRUE ~ NA_character_))
  bplot = ggplot(df, aes(x = time, y=value * 100)) + 
    geom_boxplot() + 
    geom_text_repel(aes(label = county_outlier), na.rm = TRUE, hjust = - 0.2) + 
    ylab("Percentage") + 
    xlab(NULL) + 
    # ylim(0,0.052) +
    ggtitle(paste0(title)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  return(bplot)
}

bplot_added = boxplot_change_rate(data, 3, "Added", "Added")
bplot_dropped = boxplot_change_rate(data, 3, "Dropped", "Dropped")
bplot_name_last = boxplot_change_rate(data, 3, "LastName", "Last Name Changes")
bplot_res_addr = boxplot_change_rate(data, 3, "Address", "Address Changes")
bplot_vbm_type = boxplot_change_rate(data, 3, "VbmVoterType", "VBM Type Changes")
bplot_party = boxplot_change_rate(data, 3, "PartyCode", "Party Changes")
bplot_birth_date = boxplot_change_rate(data, 3, "DOB", "Birth Date Changes")

pdf("./figures/original/bplot_added.pdf", width=12, height=12)
bplot_added
dev.off()

pdf("./figures/original/bplot_dropped.pdf", width=12, height=12)
bplot_dropped
dev.off()

pdf("./figures/original/bplot_name_last.pdf", width=12, height=12)
bplot_name_last
dev.off()

pdf("./figures/original/bplot_res_addr.pdf", width=12, height=12)
bplot_res_addr
dev.off()

pdf("./figures/original/bplot_vbm_type.pdf", width=12, height=12)
bplot_vbm_type
dev.off()

pdf("./figures/original/bplot_party.pdf", width=12, height=12)
bplot_party
dev.off()

pdf("./figures/original/bplot_birth_date.pdf", width=12, height=12)
bplot_birth_date
dev.off()

# pdf("bplot_name_addr_status.pdf", width=5, height=15)
# ggarrange(bplot_name_last, bplot_res_addr, bplot_voter_status,
#           ncol = 1, nrow = 3,
#           labels = c("A", "B", "C"), label.x = 0.01)
# dev.off()
# 
# pdf("bplot_party_dob.pdf", width=5, height=10)
# ggarrange(bplot_party, bplot_birth_date, ncol = 1, nrow = 2,
#           labels = c("A", "B"), label.x = 0.01)
# dev.off()

