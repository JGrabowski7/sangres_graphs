## Load packages

library(ggplot2)
library(reshape2)

## Load data
## Your path may be different so be sure to change that

est_data <- read.csv("S:/Ecology/Student_folders_&_files/Jonathan 2024/Sangres/Establishment_Stats_2024.csv")

---------------------------------------------------------------------------------------------------------------

## DATA BY PLOT

  
## Makes a graph of MOG density (ha) per plot

ggplot(est_data, aes(x= reorder(PlotName, MOGDensityPerHa), y= MOGDensityPerHa)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("MOG density (ha)") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph of total area per plot

ggplot(est_data, aes(x= reorder(PlotName, PlotSize.ha.), y= PlotSize.ha.)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Total area (ha)") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph of the number of trees sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, NumTrees), y= NumTrees)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of trees") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of MOG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, NumMOG), y= NumMOG)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of trees") +
  theme_classic() +
  theme(legend.title = element_blank())

--------------------------------------------------------------------------------------------------------------  

## DATA BY TREATMENT
  

## Get MOG density per ha per treatment
## Then make a graph of MOG density per treatment
  
TreatedMOGDensity <- sum(subset(est_data, TreatmentStatus == 'Treated')$MOGDensityPerHa)

UntreatedMOGDensity <- sum(subset(est_data, TreatmentStatus == 'Untreated')$MOGDensityPerHa)
  
Treatment <- c('Treated', 'Untreated')

TotalMOGDensity <- c(TreatedMOGDensity, UntreatedMOGDensity)

TotalMOGDensityByTreatment <- data.frame(Treatment, TotalMOGDensity)
  
ggplot(TotalMOGDensityByTreatment, aes(x = Treatment, y = TotalMOGDensity)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("MOG density (ha)") +
  ylim(0, 800) +
  theme_classic() +
  theme(legend.position = "none")

## Get total area of treated and untreated plots 
## Then make a graph of total area by treatment

TreatedArea <- sum(subset(est_data, TreatmentStatus == 'Treated')$PlotSize.ha.)

UntreatedArea <- sum(subset(est_data, TreatmentStatus == 'Untreated')$PlotSize.ha.)

TotalArea <- c(TreatedArea, UntreatedArea)

AreaByTreatment <- data.frame(Treatment, TotalArea)

ggplot(AreaByTreatment, aes(x = Treatment, y = TotalArea)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Total area") +
  theme_classic() +
  theme(legend.position = "none")

## Get total number of trees in treated and untreated plots
## Then make a graph of number of trees by treatment

TreatedNumTrees <- sum(subset(est_data, TreatmentStatus == 'Treated')$NumTrees)

UntreatedNumTrees <- sum(subset(est_data, TreatmentStatus == 'Untreated')$NumTrees)

NumTrees <- c(TreatedNumTrees, UntreatedNumTrees)

NumTreesByTreatment <- data.frame(Treatment, NumTrees)

ggplot(NumTreesByTreatment, aes(x = Treatment, y = NumTrees)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of trees") +
  ylim(0, 3000) +
  theme_classic() +
  theme(legend.position = "none")

## Get total number of MOG in treated and untreated plots
## Then make a graph of number of MOG per treatment

TreatedMOG <- sum(subset(est_data, TreatmentStatus == 'Treated')$NumMOG)

UntreatedMOG <- sum(subset(est_data, TreatmentStatus == 'Untreated')$NumMOG)

NumMOG <- c(TreatedMOG, UntreatedMOG)

NumMOGByTreatment <- data.frame(Treatment, NumMOG)

ggplot(NumMOGByTreatment, aes(x = Treatment, y = NumMOG)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of mature old growth") +
  ylim(0, 400) +
  theme_classic() +
  theme(legend.position = "none")

-------------------------------------------------------------------------------------------------------------

## OG and MOG by plot
  
## change the number after each to the total number of plots
  
## I'm not sure how this works but here's where I got the code from:
  ##https://stackoverflow.com/questions/10212106/creating-grouped-bar-plot-of-multi-column-data-in-r
  
condition <- rep(c("OG", "MOG"), times = 6)

data <- melt(est_data[,c('PlotName','NumOG','NumMOG')],id.vars = 1)
  
ggplot(data ,aes(x = PlotName, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
  xlab("Plot") +
  ylab("Number of trees") +
  ylim(0, 200) +
  scale_fill_discrete(labels = c('Old Growth', 'Mature Old Growth')) +
  theme_classic() +
  theme(legend.title = element_blank())
  
  