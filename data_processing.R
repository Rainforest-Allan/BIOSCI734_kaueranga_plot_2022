setwd("~/BIOSCI734_kaueranga_plot_2022")
library(tidyverse)
library(grid)
library(broom)
library(cowplot)
library(gridExtra)
library(patchwork)

rawdata <- read_csv("treedata.csv")
view(rawdata)
rawdata <- mutate(rawdata, id = row_number(), .before = transect)
data <- rawdata %>% select(-contains(c("trans", "tag")))
view(data)

#cleaning data for Ranalysis for year and basal area

testdata <- data %>% select(-contains(c("diam", "rad", "cross"))) %>%
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
  group_by(veg_type, year) %>% mutate(ba_per_type = sum(basal_area)) %>%
  ungroup() 

view(testdata)
view(testdata2)

freqdata <- testdata2 %>% 
  mutate(fr1993 = ifelse(status93 %in% "A", 1, 0)) %>%
  mutate(fr2013 = ifelse(status13 %in% "A", 1, 0)) %>%
  mutate(fr2022 = ifelse(status13 %in% "A", 1, 0)) %>%
  select(-contains(c("status"))) %>%
  select(species, fr1993, fr2013, fr2022, id) %>%
  pivot_longer(cols = c(`fr1993`, `fr2013`, `fr2022`), 
               names_to = "year",
               names_pattern = "fr(....)",
               values_to = "frequency",
               names_repair = "minimal") %>%
  group_by(species, year) %>%
  summarise(frequency = sum(frequency)) %>%
  group_by(species, year) %>% mutate(frequency = frequency/3)

view(freqdata)

standdensity <- freqdata %>%
          group_by(species, year) %>%
          filter(n_distinct(species)==1) %>% 
          distinct() %>%
          mutate(tree_density = c(1)) %>%
          summarise(tree_density = sum(tree_density))

standdensity <- freqdata %>%
  select(species, year, fr93, fr13, fr22) %>%
  filter(n_distinct(species)==1) %>% 
  distinct() %>%
  mutate(tree_density = c(1)) %>%
  summarise(tree_density = sum(tree_density))


view(standdensity)

# standdensity <- testdata %>%
#   select(species, year, status93, status13, status22) %>%
#   group_by(year, species, status93) %>%
#   filter(status93 == "A") %>% 
#   ungroup() %>%
#   group_by(year, species, status13) %>%
#   ungroup() %>%
#   filter(status13 == "A") %>%
#   group_by(year, species, status22) %>%
#   filter(status22 == "A") %>%
#   ungroup() %>%
#   distinct() %>%
#   mutate(tree_density = c(1)) %>%
#   summarise(tree_density = sum(tree_density))

meandata <- testdata %>% select(species, year, basal_area) %>%
  group_by(species, year) %>% mutate(meanba_perspecies = mean(basal_area)) %>%
  select(species, year, meanba_perspecies) %>%
  group_by(species, year) %>%
  distinct()
  
  
meanplotdata <- testdata %>% select(species, year, basal_area) %>%
    group_by(species, year) %>% mutate(meanba_perspecies = mean(basal_area)) %>%
    select(species, year, meanba_perspecies) %>%
    group_by(species, year) %>%
    summarise(meanba_perspecies = sum(meanba_perspecies))

vegdata <- testdata %>% select(veg_type, year, basal_area) %>%
  group_by(veg_type, year) %>%  mutate(ba_per_type = sum(basal_area)) %>% 
  group_by(veg_type, year) %>% mutate(total = sum(ba_per_type)) %>%
  summarise(total = sum(total)) %>%
  mutate(logtotal = log(total))

  

view(data)

view(meandata)
view(meanplotdata)
view(vegdata)


basalplot <- ggplot(testdata2, aes(x = year, y = basal_area)) + 
  geom_jitter(aes(colour = species, shape = species, size = 0.07, alpha = 1/50), width = 0.2, height = 0) +
  scale_shape_manual(values = c(0:3, 5,6, 7, 9, 10, 12, 15:25)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Basal area"(~m^2 ~ha^-1 ))) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Species"), shape = guide_legend(title = "Clade")) +
  scale_x_discrete(limits = c('1993', '2003', '2013',  '2022')) 
  
basalplot 

trendplot <- ggplot(testdata2, aes(x = year, y = basal_area)) + 
  geom_point(data = vegdata, aes(x = year, y = logtotal, shape = veg_type, size = 2)) + 
  scale_shape_manual(values = c(22,21,24)) +
  geom_smooth(data = meandata, method = "lm", formula = y ~ log(x), se = FALSE, fullrange = TRUE, aes( x = year, y = meanba_perspecies, group = species, colour = species)) + 
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Basal area"(~m^2 ~ha^-1 ))) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Average basal area per species"), shape = guide_legend(title = "Log average basal area by each Clade")) +
  scale_x_discrete(limits = c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023')) +
  theme(axis.text.x = element_text(angle = 90))
trendplot



meanplot <- ggplot(meandata, aes(x = year, y = meanba_perspecies)) + 
  geom_jitter(aes(colour = species, shape = species, size = 0.07), width = 0.2, height = 0) +
  scale_shape_manual(values = c(0:3, 5,6, 7, 9, 10, 12, 15:25)) +
  geom_smooth() +
  geom_smooth( method = "lm", formula = y ~ log(x), se = FALSE, fullrange = TRUE, aes( x = year, y = meanba_perspecies, group = species, colour = species)) +
  theme_classic()  +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Basal area"(~m^2 ~ha^-1 ))) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Average basal area per species"), shape = guide_legend(title = "Log average basal area by each Clade"))
meanplot 

densityplot <- ggplot(freqdata, aes(x = year, y = frequency, group = species)) + geom_point(aes(colour = species, size = 2)) +
  geom_line(aes(colour = species)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Total frequency of plants recorded")) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Plant species recorded"), shape = guide_legend(title = "Clade")) +
  scale_x_discrete(limits = c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023')) +
  theme(axis.text.x = element_text(angle = 90))
densityplot

Ldensityplot <- ggplot(freqdata[freqdata$frequency < 30,], aes(x = year, y = frequency, group = species)) + geom_point(aes(colour = species, size = 2)) +
  geom_line(aes(colour = species)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  xlab("Year") + ylab(bquote("Total frequency of plants recorded")) +
  guides(size = "none", alpha = "none", colour = guide_legend(title = "Plant species recorded"), shape = guide_legend(title = "Clade")) +
  scale_x_discrete(limits = c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023')) +
  theme(axis.text.x = element_text(angle = 90))
Ldensityplot

