setwd("~/BIOSCI734_kaueranga_plot_2022")
library(tidyverse)
library(grid)
library(broom)
library(cowplot)
library(gridExtra)
library(patchwork)

rawdata <- read_csv("treedata.csv")
view(rawdata)

#cleaning data for Ranalysis for year and basal area
rawdata <- tibble::rowid_to_column(data, "id")
rawdata <- as.character("id")

testdata <- rawdata %>% select(-contains(c("diam", "rad", "cross"))) %>%
  pivot_longer(cols = c(`basal_area_1993`, `basal_area_2013`, `basal_area_2022`), 
                                     names_to = "year",
                                     names_pattern = "basal_area_(....)",
                                     values_to = "basal_area",
                                     names_repair = "minimal") %>%
  mutate(veg_type = ifelse(species %in% "Cyathea dealbata","Pteridophyte", 
                           ifelse(species %in% "Pterophylla sylvicola","Angiosperm", 
                                  ifelse(species %in% "Beilschmiedia tawa","Gymnosperm", 
                                         ifelse(species %in% "Coprosma grandifolia","Angiosperm", 
                                                ifelse(species %in% "Cyathea medullaris","Pteridophyte",
                                                       ifelse(species %in% "Cyathea smithii", "Pteridophyte",
                                                              ifelse(species %in% "Dacrydium cupressinum","Gymnosperm",
                                                                     ifelse(species %in% "Dicksonia squarrosa", "Pteridophyte",
                                                                            ifelse(species %in% "Geniostoma ligustrifolium", "Angiosperm",
                                                                                   ifelse(species %in% "Knightia excelsa", "Angiosperm",
                                                                                          ifelse(species %in% "Kunzea robusta", "Angiosperm",
                                                                                                 ifelse(species %in% "Leptospermum scoparium", "Angiosperm",
                                                                                                        ifelse(species %in% "Leucopogon fasciculatus", "Angiosperm",
                                                                                                               ifelse(species %in% "Lophomyrtus bullata", "Angiosperm",
                                                                                                                      ifelse(species %in% "Olearia rani", "Angiosperm",
                                                                                                                             ifelse(species %in% "Pseudopanax arboreus", "Angiosperm", 
                                                                                                                                    ifelse(species %in% "Pseudopanax crassifolius", "Angiosperm",
                                                                                                                                           ifelse(species %in% "Pseudopanax discolor","Angiosperm","")))))))))))))))))), .after = "species") 

testdata <- testdata[-c(2308,2309,2310),]
testdata2 <- testdata %>% group_by(species, year) %>% mutate(ba_per_species = sum(basal_area)) %>% 
  group_by(species, veg_type) %>% mutate(ba_per_type = sum(basal_area)) %>%
  ungroup() 


filter(n_distinct(species)==1) %>% 
  distinct() %>%
  mutate(tree_density = c(1)) %>%
  summarise(tree_density = sum(tree_density))

view(testdata)
view(testdata2)
standdensity <- testdata %>%
          group_by(species, year) %>%
          filter(n_distinct(species)==1) %>% 
          distinct() %>%
          mutate(tree_density = c(1)) %>%
          summarise(tree_density = sum(tree_density))

meandata <- testdata %>% select(species, year, basal_area) %>%
  group_by(species, year) %>% mutate(meanba_perspecies = mean(basal_area)) %>%
  select(species, year, meanba_perspecies) %>%
  distinct()

  
view(data)
view(standdensity)
view(meandata)


basalplot <- ggplot(testdata2, aes(x = year, y = basal_area)) + 
  geom_jitter(aes(colour = species, shape = veg_type, size = 0.07, alpha = 1/50), width = 0.2, height = 0) +
  geom_smooth(data = meandata, method = "lm", formula = y ~ log(x), se = FALSE, fullrange = TRUE, aes( x = year, y = meanba_perspecies, group = species, colour = species, size = 0.05)) + 
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Basal area"(~m^2 ~ha^-1 ))) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Species"), shape = guide_legend(title = "Clade"))
basalplot 


