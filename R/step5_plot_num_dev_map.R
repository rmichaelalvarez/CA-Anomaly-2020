
num_dev = readRDS("./data/num_dev.Rds")
county_abb = read.csv("./data/ca_county_abb.csv", stringsAsFactors = F)

# install.packages(c("ggplot2", "magrittr", "tidyverse",
#                    "sf", "maps", "RColorBrewer", "rnaturalearth"))
library(ggplot2)
library(magrittr)
library(tidyverse)
library(sf)
library(maps)
library(RColorBrewer)
library(rnaturalearth)

# Colors
color_list = brewer.pal(9, "Greys")

# World map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Preparing county info
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>%
  subset(grepl("california", ID)) %>%
  mutate(
    area = as.numeric(st_area(.)),
    County = str_remove(ID, "california,"),
    County_abb = county_abb[match(County, tolower(county_abb$County)), "Abb"],
    deviations = num_dev[match(County, tolower(num_dev$county)),"n"],
    lon = map_dbl(geom, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geom, ~st_centroid(.x)[[2]])
  )

# Map + Text
figure = ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, aes(fill = factor(deviations))) +
  scale_fill_grey(start = 0.9, end = 0, guide=FALSE) +
  geom_text(data=counties,
            size = 2.5,
            aes(x = lon, y = lat, label=County_abb, colour = factor(deviations))) +
  # scale_colour_grey(start = 0, end = 0.9) +
  scale_colour_manual(values=c("0"="black","1"="black","2"="black","3"="black","4"="black",
                               "5"="black","6"="black","8"="black","9"="black","10"="black",
                               "11"="white","12"="white","13"="white","15"="white",
                               "19"="white"), guide=FALSE) +
  coord_sf(xlim = c(-127,-112), ylim = c(31.5, 43), expand = FALSE) + 
  xlab("") + 
  ylab("") +
  theme_bw() +
  guides(fill=guide_legend(title="# of Deviations"))

pdf("./figures/original/number_of_deviations_map.pdf", width=8, height=8)
figure
dev.off()


# two dimension scatter plot
t_test = readRDS("./data/t_test.Rds")
controller = readRDS("./data/controller.Rds")

expenditure = controller %>% 
  do.call("rbind",.) %>% 
  filter(election_cost > 0) %>% 
  group_by(name) %>% 
  summarize(total_exp = mean(total_exp),
            election_cost = mean(election_cost),
            election_prop = mean(election_prop)) %>% 
  mutate(County = tolower(name))

counties = left_join(counties,
                     t_test %>% group_by(County) %>%
                       summarise(N = mean(VoterCount)) %>% 
                       mutate(County = tolower(County)),
                     by = "County")

counties = left_join(counties,
                     expenditure,
                     by = "County")

counties$log_N = log(counties$N)
counties$log_area = log(counties$area)

figure_scatter = counties %>% 
  # filter(!is.na(election_prop)) %>%
  ggplot(aes(x = log_area, y = deviations)) +
  geom_point() + 
  geom_text(size = 2.5,
            nudge_y = 0.5,
            aes(label=County_abb))

res = lm(deviations ~ log_area + log_N + election_prop, data = counties)
summary(res)

figure_scatter = df %>% 
  ggplot(aes(x = cost_per_person, y = outlier)) +
  geom_point() + 
  geom_text(size = 2.5,
            nudge_y = 0.5,
            aes(label=county))