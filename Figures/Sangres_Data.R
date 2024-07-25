## Load packages

library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)


###CJM's data prep 

#Read in excel sheets.  Probably should be using csvs but I'm lazy.  See how this goes
SFS4 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sfs4 bible 2024.xlsx")
#NOTE: should get BTN4 species as well
BTN4 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/btn4 revisit data 2024.xlsx")
SFF1 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff1 establishment data.xlsx")
SFF2 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff3 establishment data.xlsx")
SFF5 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff5 establishment data 2024.xlsx")
SFF8 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff8 establishment data.xlsx")
SFF10 <- read_excel("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff10 initial 2024 data.xlsx")

# I'm just gonna add this so it's easier for me to run code - Jonathan

SFS4 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sfs4 bible 2024.xlsx")
BTN4 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/btn4 revisit data 2024.xlsx")
SFF1 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff1 establishment data.xlsx")
SFF2 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff3 establishment data.xlsx")
SFF5 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff5 establishment data 2024.xlsx")
SFF8 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff8 establishment data.xlsx")
SFF10 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff10 initial 2024 data.xlsx")
  
#Fix janky column name in SFF2
names(SFF2)[5] <- paste("DBH")

#Add plot numbers to treeIDs

SFS4$Tree_num = paste0('SFS4-', SFS4$Tree_num)
BTN4$Tree_num = paste0('BTN4-', BTN4$Tree_num)
SFF1$Tree_num = paste0('SFF1-', SFF1$Tree_num)
SFF2$Tree_num = paste0('SFF2-', SFF2$Tree_num)
SFF3$Tree_num = paste0('SFF3-', SFF3$Tree_num)
SFF5$Tree_num = paste0('SFF5-', SFF5$Tree_num)
SFF8$Tree_num = paste0('SFF8-', SFF8$Tree_num)
SFF10$Tree_num = paste0('SFF10-', SFF10$Tree_num)

#Add plot number to data
SFS4$PlotName <- "SFS4"
BTN4$PlotName <- "BTN4"
SFF1$PlotName <- "SFF1"
SFF2$PlotName <- "SFF2"
SFF3$PlotName <- "SFF3"
SFF5$PlotName <- "SFF5"
SFF8$PlotName <- "SFF8"
SFF10$PlotName <- "SFF10"

#Add treatment status

SFS4$TreatmentStatus <- "Treated"
BTN4$TreatmentStatus <- "Untreated"
SFF1$TreatmentStatus <- "Treated"
SFF2$TreatmentStatus <- "Untreated"
SFF3$TreatmentStatus <- "Untreated"
SFF5$TreatmentStatus <- "Treated"
SFF8$TreatmentStatus <- "Treated"
SFF10$TreatmentStatus <- "Treated"

#Add plot size
SFS4$PlotSize <- 1
BTN4$PlotSize <- 1
SFF1$PlotSize <- 0.25
SFF2$PlotSize <- 1
SFF3$PlotSize <- 0.25
SFF5$PlotSize <- 0.25
SFF8$PlotSize <- 1
SFF10$PlotSize <- 0.25
  
#Select columns of interest (I'm begging someone to write a loop, this is so embarrasing)

SFS4 <- SFS4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
BTN4 <- BTN4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF1 <- SFF1 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF10 <- SFF10 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF8 <- SFF8 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF2 <- SFF2 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF3 <- SFF3 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF5 <- SFF5 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)

#Okay well, now we merge all the plots into one dataframe
SFS4$Condition <- as.numeric(SFS4$Condition)
merged_plots <- bind_rows(SFS4, BTN4, SFF1, SFF10, SFF8, SFF2, SFF3, SFF5)

#Now we add a column to define MOG as Y or NA
merged_plots <- merged_plots %>%
  mutate(MOG = case_when(OG == "Y" | DBH > 30 ~ "Y"))
             
#calculate tree counts
merged_summary <- merged_plots %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(Tree_count = n(), meanDBH = mean(DBH, na.rm = TRUE))

#calculate MOG stats
merged_MOG <- merged_plots %>%
  filter(MOG == "Y")%>%
  group_by(PlotName, PlotSize, TreatmentStatus)%>%
  summarize(MOG_count = n())

#calculate live MOG stats
merged_MOG_live <- merged_plots %>%
  filter(MOG == "Y" & Condition != 5 & Condition != 2) %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(MOG_live_count = n())

#calculate OG stats
merged_OG <- merged_plots %>%
  filter(OG == "Y") %>%
  group_by(PlotName, PlotSize, TreatmentStatus) %>%
  summarize(OG_count = n())

#merge em all together.  probably a better way to do this, but here we are
merged_summary$MOG_count <- merged_MOG$MOG_count
merged_summary$MOG_live_count <- merged_MOG_live$MOG_live_count
merged_summary$OG_count <- merged_OG$OG_count

#Calculate TPH
merged_summary$TPH <- merged_summary$Tree_count/merged_summary$PlotSize
est_data<-merged_summary


###Okay, now for some treatment level stats
treatment_summary <- merged_summary %>%
  group_by(TreatmentStatus)%>%
  summarize(Tree_count = sum(Tree_count), MOG_count = sum(MOG_count), MOG_live_count = sum(MOG_live_count), Surveyed_ha = sum(PlotSize))

#Jonathan, see "treatment_summary" and "merged_summary" dataframes.  These will have the data you'll want to make figures!
#I updated some variable names to match your figure code, but you may have to change or calculate a few
#I think MOG and OG density is less important to visualize than MOG and OG numbers alone, since sample size is what we are interested in
# Note I added stats for live MOGs.  We will want to plot this data as this is also an important sample subset we're interested in (tracking MOG that have capacity to die later!)
---------------------------------------------------------------------------------------------------------------

## DATA BY PLOT

## Makes a graph of total area per plot

ggplot(est_data, aes(x= reorder(PlotName, PlotSize), y= PlotSize)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Total area (ha)") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph of the number of trees sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, Tree_count), y= Tree_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of trees") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of MOG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, MOG_count), y= MOG_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of mature old growth") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of live MOG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, MOG_live_count), y= MOG_live_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of live mature old growth") +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of OG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, OG_count), y= OG_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Number of old growth") +
  ylim(0,200) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of MOG per ha per plot

MOGperHA <- est_data$MOG_count / est_data$PlotSize

ggplot(est_data, aes(x= reorder(PlotName, MOGperHA), y= MOGperHA)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("Plot") +
  ylab("Mature old growth density (trees per ha)") +
  ylim(0,200) +
  theme_classic() +
  theme(legend.title = element_blank())

--------------------------------------------------------------------------------------------------------------  

## DATA BY TREATMENT

## Get total area of treated and untreated plots 
## Then make a graph of total area by treatment

TreatedArea <- sum(subset(est_data, TreatmentStatus == 'Treated')$PlotSize)

UntreatedArea <- sum(subset(est_data, TreatmentStatus == 'Untreated')$PlotSize)

TotalArea <- c(TreatedArea, UntreatedArea)

Treatment <- c("Treated", "Untreated")

AreaByTreatment <- data.frame(Treatment, TotalArea)

ggplot(AreaByTreatment, aes(x = Treatment, y = TotalArea)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Total area") +
  theme_classic() +
  theme(legend.position = "none")

## Get total number of trees in treated and untreated plots
## Then make a graph of number of trees by treatment

TreatedNumTrees <- sum(subset(est_data, TreatmentStatus == 'Treated')$Tree_count)

UntreatedNumTrees <- sum(subset(est_data, TreatmentStatus == 'Untreated')$Tree_count)

NumTrees <- c(TreatedNumTrees, UntreatedNumTrees)

NumTreesByTreatment <- data.frame(Treatment, NumTrees)

ggplot(NumTreesByTreatment, aes(x = Treatment, y = NumTrees)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of trees") +
  ylim(0, 4000) +
  theme_classic() +
  theme(legend.position = "none")

## Get total number of MOG in treated and untreated plots
## Then make a graph of number of MOG per treatment

TreatedMOG <- sum(subset(est_data, TreatmentStatus == 'Treated')$MOG_count)

UntreatedMOG <- sum(subset(est_data, TreatmentStatus == 'Untreated')$MOG_count)

NumMOG <- c(TreatedMOG, UntreatedMOG)

NumMOGByTreatment <- data.frame(Treatment, NumMOG)

ggplot(NumMOGByTreatment, aes(x = Treatment, y = NumMOG)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of mature old growth") +
  ylim(0, 500) +
  theme_classic() +
  theme(legend.position = "none")

## Get number of live MOG's in treated and untreated plots
## Then make a graph of the number of live MOG per treatment

TreatedLiveMOG <- sum(subset(est_data, TreatmentStatus == 'Treated')$MOG_live_count)

UntreatedLiveMOG <- sum(subset(est_data, TreatmentStatus == 'Untreated')$MOG_live_count)

NumLiveMOG <- c(TreatedLiveMOG, UntreatedLiveMOG)

NumLiveMOGbyTreatment <- data.frame(Treatment, NumLiveMOG)

ggplot(NumMOGByTreatment, aes(x = Treatment, y = NumLiveMOG)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of live mature old growth") +
  ylim(0, 400) +
  theme_classic() +
  theme(legend.position = "none")

## Get number of OG's in the treated and untreated pots
## Then make a graph of the number of OG's per treatment

TreatedOG <- sum(subset(est_data, TreatmentStatus == 'Treated')$OG_count)
  
UntreatedOG <- sum(subset(est_data, TreatmentStatus == 'Untreated')$OG_count)
  
NumOG <- c(TreatedOG, UntreatedOG)
  
NumOGbyTreatment <- data.frame(Treatment, NumOG)

ggplot(NumMOGByTreatment, aes(x = Treatment, y = NumOG)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  ylab("Number of old growth") +
  ylim(0, 250) +
  theme_classic() +
  theme(legend.position = "none")

## Makes a graph showing the number of MOG per ha per plot



-------------------------------------------------------------------------------------------------------------

## OTHER COOL GRAPHS 
  
  
## OG and MOG by plot
  
## I'm not sure how this works but here's where I got the code from:
  ##https://stackoverflow.com/questions/10212106/creating-grouped-bar-plot-of-multi-column-data-in-r

OGvsMOG <- melt(est_data[,c('PlotName','OG_count','MOG_count')],id.vars = 1)
  
ggplot(OGvsMOG ,aes(x = PlotName, y = value)) + 
  geom_bar(aes(fill = variable), stat = "identity", color = 'black', position = "dodge") +
  xlab("Plot") +
  ylab("Number of trees") +
  ylim(0, 200) +
  scale_fill_manual(values=c("yellow", "blue"), labels = c('Old Growth', 'Mature Old Growth')) +
  theme_classic() +
  theme(legend.title = element_blank())


-------------------------------------------------------------------------------------------------------
## Plot-level Histograms & Other fun things Carolina is playing around with
  
  #all trees by plot (species-color-coded)
  ggplot(merged_plots, aes(x=DBH)) + geom_histogram()
# Change the width of bins
ggplot(merged_plots, aes(x=DBH, fill = Species, color = Species)) + 
  geom_histogram(binwidth=2)+
  xlab("DBH (cm)")+
  facet_wrap(~PlotName)

#same but facet by treatment status
#all trees by plot (species-color-coded)
ggplot(merged_plots, aes(x=DBH)) + geom_histogram()
# Change the width of bins
ggplot(merged_plots, aes(x=DBH, fill = Species, color = Species)) + 
  geom_histogram(binwidth=2)+
  xlab("DBH (cm)")+
  facet_wrap(~TreatmentStatus)

merged_plots$Condition <- as.factor(merged_plots$Condition)
#all trees by plot (live vs. dead)
ggplot(merged_plots, aes(x=DBH)) + geom_histogram()
# Change the width of bins
ggplot(merged_plots, aes(x=DBH, fill = Condition, color = Condition)) + 
  geom_histogram(binwidth=2)+
  xlab("DBH (cm)")+
  facet_wrap(~TreatmentStatus)
  