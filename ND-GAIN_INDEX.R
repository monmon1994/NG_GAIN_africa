# ND-GAIN INdex Rankings for Africa
# For the AIF environment edition giving an overview of climate change readiness

library(tidyverse)
library(wesanderson)
library(countrycode)
library(car)
library(readr)
library(gghighlight)
library(extrafont)
library(ggrepel)
loadfonts(device = "win", quiet = T)
# Load the data

readi_df_ts <- read_csv("data/resources/readiness/readiness.csv")

vul_df_ts <- read_csv("data/resources/vulnerability/vulnerability.csv")

pal <- wesanderson::wes_palette("Darjeeling1", n = 192, type = "continuous")

# summary of the data

summary(readi_df_ts)

plot(readi_df_ts$`2017`)

hist(readi_df_ts$`2017`)

# Africa only

readi_africa <- readi_df_ts %>% 
  filter(ISO3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                     "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                     "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                     "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                     "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>% 
  gather(year, readiness, '1995':'2017') 

vul_africa <- vul_df_ts %>% 
  filter(ISO3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                     "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                     "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                     "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                     "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>% 
  gather(year, vulnerability, '1995':'2017') 


readi_africa$year <- as.numeric(readi_africa$year)

vul_africa$year <- as.numeric(vul_africa$year)

merged <- merge(readi_africa, vul_africa, by = "ISO3")

# whole world

readi <- readi_df_ts %>% 
  gather(year, readiness, '1995':'2017') %>% 
  distinct() %>% 
  filter(year == 2017)
  
vul <- vul_df_ts %>% 
  gather(year, vulnerability, '1995':'2017') %>% 
  distinct() %>% 
  filter(year == 2017)

merged <- merge(readi, vul, by = "ISO3")


label <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
             "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
             "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
             "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
             "TGO", "TUN", "UGA", "ZMB", "ZWE")

# Readiness and Vul africa vs the world 

breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8)

ggplot(merged) +
  geom_point(aes(x = readiness, y = vulnerability, color = Name.x), position = "jitter", stat = "unique", size = 2) +
  geom_label_repel(data = merged %>% filter(ISO3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                                                  "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                                                  "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                                                  "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                                                  "TGO", "TUN", "UGA", "ZMB", "ZWE")), 
             aes(x = readiness, y = vulnerability, label = ISO3), label.size = 0.1, label.padding = 0.2, 
             segment.size = 0.1, segment.alpha = 0.5, segment.color = "black") +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = breaks, labels = breaks) +
  scale_y_continuous(breaks = breaks, labels = breaks) +
  geom_vline(xintercept = .37, linetype = 3, size = 1) +
  geom_hline(yintercept = .42, linetype = 3, size = 1) +
  theme_light(base_size = 12, base_family = "Georgia") +
  labs(title = "ND-GAIN Country Index on Climate Change Vulnerability and Resilience",
       caption = "Source: Notre Dame Global Adaptation Initiative 2020",
       x = "Readiness", y = "Vulnerability") +
  theme(text = element_text(family = "Georgia")) +
  guides(color = F)


  


