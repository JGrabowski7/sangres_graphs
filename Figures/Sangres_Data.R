## Load packages

library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(gridExtra)


###CJM's data prep 

#Read in excel sheets.  Probably should be using csvs but I'm lazy.  See how this goes
SFS4 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sfs4 bible 2024.xlsx")
#NOTE: should get BTN4 species as well
BTN4 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/btn4 revisit data 2024.xlsx")
BTN4dbh <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/btn4 access for C.xlsx")
SFF1 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff1 establishment data.xlsx")
SFF2 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff3 establishment data.xlsx")
SFF4 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff4 establishment data.xlsx")
SFF5 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff5 establishment data 2024.xlsx")
SFF7 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff7 establishment data 2024.xlsx")
SFF8 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff8 establishment data.xlsx")
SFF9 <- read_excel ("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff9 establishment data.xlsx")
SFF10 <- read_excel("C:/Users/cjmay/Documents/GitHub/sangres_graphs/Data/sff10 initial 2024 data.xlsx")

# I'm just gonna add this so it's easier for me to run code - Jonathan

SFS4 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sfs4 bible 2024.xlsx")
BTN4 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/btn4 revisit data 2024.xlsx")
BTN4dbh <- read_excel ("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/btn4 access for C.xlsx")
SFF1 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff1 establishment data.xlsx")
SFF2 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff3 establishment data.xlsx")
SFF4 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff4 establishment data.xlsx")
SFF5 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff5 establishment data 2024.xlsx")
SFF8 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff8 establishment data.xlsx")
SFF9 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff9 establishment data.xlsx")
SFF10 <- read_excel("S:/Ecology/Student_folders_&_files/Jonathan 2024/sangres_graphs/Data/sff10 initial 2024 data.xlsx")

SFS4 <- read_excel("Data/sfs4 bible 2024.xlsx")
BTN4 <- read_excel("Data/btn4 revisit data 2024.xlsx")
BTN4dbh <- read_excel ("Data/btn4 access for C.xlsx")
SFF1 <- read_excel("Data/sff1 establishment data.xlsx")
SFF2 <- read_excel("Data/sff2 establishment 2024 data.xlsx")
SFF3 <- read_excel("Data/sff3 establishment data.xlsx")
SFF4 <- read_excel("Data/sff4 establishment data.xlsx")
SFF5 <- read_excel("Data/sff5 establishment data 2024.xlsx")
SFF6 <- read_excel("Data/sff6 establishment data.xlsx")
SFF7 <- read_excel("Data/sff7 establishment data.xlsx")
SFF8 <- read_excel("Data/sff8 establishment data.xlsx")
SFF9 <- read_excel("Data/sff9 establishment data.xlsx")
SFF10 <- read_excel("Data/sff10 initial 2024 data.xlsx")


#Fix janky column name in SFF2
names(SFF2)[5] <- paste("DBH")

#Add plot numbers to treeIDs

SFS4$Tree_num = paste0('SFS4-', SFS4$Tree_num)
BTN4$Tree_num = paste0('BTN4-', BTN4$Tree_num)
SFF1$Tree_num = paste0('SFF1-', SFF1$Tree_num)
SFF2$Tree_num = paste0('SFF2-', SFF2$Tree_num)
SFF3$Tree_num = paste0('SFF3-', SFF3$Tree_num)
SFF4$Tree_num = paste0('SFF4-', SFF4$Tree_num)
SFF5$Tree_num = paste0('SFF5-', SFF5$Tree_num)
SFF6$Tree_num = paste0('SFF6-', SFF6$Tree_num)
SFF7$Tree_num = paste0('SFF7-', SFF7$Tree_num)
SFF8$Tree_num = paste0('SFF8-', SFF8$Tree_num)
SFF9$Tree_num = paste0('SFF9-', SFF9$Tree_num)
SFF10$Tree_num = paste0('SFF10-', SFF10$Tree_num)

#do a quick n dirty join of BTN4 DBH & Species files
BTN4dbh$Tree_num <- BTN4dbh$TreeID
BTN4 <- left_join(BTN4, BTN4dbh, by = "Tree_num")
BTN4$Species <- BTN4$SpeciesID
BTN4$Notes<-BTN4$Notes.x


#Add plot number to data
SFS4$PlotName <- "SFS4"
BTN4$PlotName <- "BTN4"
SFF1$PlotName <- "SFF1"
SFF2$PlotName <- "SFF2"
SFF3$PlotName <- "SFF3"
SFF4$PlotName <- "SFF4"
SFF5$PlotName <- "SFF5"
SFF6$PlotName <- "SFF6"
SFF7$PlotName <- "SFF7"
SFF8$PlotName <- "SFF8"
SFF9$PlotName <- "SFF9"
SFF10$PlotName <- "SFF10"

#Add treatment status

SFS4$TreatmentStatus <- "Treated"
BTN4$TreatmentStatus <- "Untreated"
SFF1$TreatmentStatus <- "Treated"
SFF2$TreatmentStatus <- "Untreated"
SFF3$TreatmentStatus <- "Untreated"
SFF4$TreatmentStatus <- "Untreated"
SFF5$TreatmentStatus <- "Treated"
SFF6$TreatmentStatus <- "Untreated"
SFF7$TreatmentStatus <- "Treated"
SFF8$TreatmentStatus <- "Treated"
SFF9$TreatmentStatus <- "Untreated"
SFF10$TreatmentStatus <- "Treated"

#Add plot size
SFS4$PlotSize <- 1
BTN4$PlotSize <- 1
SFF1$PlotSize <- 0.25
SFF2$PlotSize <- 1
SFF3$PlotSize <- 0.25
SFF4$PlotSize <- 0.25
SFF5$PlotSize <- 0.25
SFF6$PlotSize <- 0.25
SFF7$PlotSize <- 0.25
SFF8$PlotSize <- 1
SFF9$PlotSize <- 0.25
SFF10$PlotSize <- 0.25
  
#Select columns of interest (I'm begging someone to write a loop, this is so embarrasing)

SFS4 <- SFS4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
BTN4 <- BTN4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF1 <- SFF1 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF2 <- SFF2 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF3 <- SFF3 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF4 <- SFF4 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF5 <- SFF5 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)
SFF6 <- SFF6 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF7 <- SFF7 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF8 <- SFF8 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)
SFF9 <- SFF9 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG,  Notes)
SFF10 <- SFF10 %>%
  select(PlotName, PlotSize, TreatmentStatus, Tree_num, Species, Condition, DBH, OG, Notes)


#Okay well, now we merge all the plots into one dataframe
SFS4$Condition <- as.numeric(SFS4$Condition)
merged_plots <- bind_rows(SFS4, BTN4, SFF1, SFF2, SFF3, SFF4, SFF5, SFF6, SFF7, SFF8, SFF9, SFF10)

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

# Add tree colors 

add_tree_colors <- function(df){
  df <- df%>%
    mutate(color=
             case_when(Species == "ABCO" ~ "#4EDFC7",
                       Species == "ACGL" ~ "#C21E56",
                       Species == "JUMO" ~ "#FFC0CB",
                       Species == "JUSC" ~ "#95658B",
                       Species == "PIED" ~ "#FFD700",
                       Species == "PIPO" ~ "#2E8B57",
                       Species == "PIST" ~ "#89CFF0",
                       Species == "PRVI" ~ "#9F2B68",
                       Species == "PSME" ~ "#808080",
                       Species == "QUGA" ~ "#5D3FD3",
                       Species == "QUUN" ~ "#CC5500",
                       Species == "SASC" ~ "#E3963E",
                       Species == "unknown" ~ "#AFE1AF",
                       Species == "NA" ~ "#808080"
                       
             ))
  
}

merged_plots <- add_tree_colors(merged_plots)

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
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph of the number of trees sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, Tree_count), y= Tree_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("") +
  ylab("Number of trees") +
  ylim(0, 2000) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of MOG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, MOG_count), y= MOG_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("") +
  ylab("Number of mature old growth") +
  ylim(0,200) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of live MOG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, MOG_live_count), y= MOG_live_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("") +
  ylab("Number of live mature old growth") +
  ylim(0, 200) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of OG sampled per plot

ggplot(est_data, aes(x= reorder(PlotName, OG_count), y= OG_count)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("") +
  ylab("Number of old growth") +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  ylim(0,200) +
  theme_classic() +
  theme(legend.title = element_blank())

## Makes a graph showing the number of MOG per ha per plot

MOGperHectare <- est_data$MOG_count / est_data$PlotSize

est_data$MOGperha <- MOGperHectare 

ggplot(est_data, aes(x= reorder(PlotName, MOGperha), y= MOGperha)) +
  geom_bar(stat="identity", color = 'black', aes(fill = TreatmentStatus)) +
  xlab("") +
  ylab("Mature old growth density (trees per ha)") +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
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
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
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
  xlab("") +
  ylab("Number of trees") +
  ylim(0, 5000) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
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
  xlab("") +
  ylab("Number of mature old growth") +
  ylim(0, 600) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
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
  xlab("") +
  ylab("Number of live mature old growth") +
  ylim(0, 600) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
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
  xlab("") +
  ylab("Number of old growth") +
  ylim(0, 300) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.position = "none")

## Makes a graph showing the number of MOG per ha per plot

TreatedMOGperha <- sum(subset(est_data, TreatmentStatus == 'Treated')$MOGperha)

UntreatedMOGperha <- sum(subset(est_data, TreatmentStatus == 'Untreated')$MOGperha)

NumMOGperha <- c(TreatedMOGperha, UntreatedMOGperha)

NumMOGperhaByTreatment <- data.frame(Treatment, NumMOGperha)

ggplot(NumMOGperhaByTreatment, aes(x = Treatment, y = NumMOGperha)) +
  geom_bar(stat = "identity", color = 'black', aes(fill = Treatment)) +
  xlab("") +
  ylab("Mature old growth per hectare") +
  ylim(0, 1000) +
  scale_fill_manual(values=c("#00aedb", "#d11141")) +
  theme_classic() +
  theme(legend.position = "none")

-------------------------------------------------------------------------------------------------------------

## OTHER COOL GRAPHS 
  
  
## OG and MOG by plot

TreatedPlotName <- subset(est_data, TreatmentStatus == "Treated")$PlotName
TreatedMOG <- subset(est_data, TreatmentStatus == "Treated")$MOG_count
TreatedOG <- subset(est_data, TreatmentStatus == "Treated")$OG_count

Treated_MOG_OG <- data.frame(TreatedPlotName, TreatedMOG, TreatedOG)

## I'm not sure how this works but here's where I got the code from:
##https://stackoverflow.com/questions/10212106/creating-grouped-bar-plot-of-multi-column-data-in-r

T_MOG_OG <- melt(Treated_MOG_OG[,c('TreatedPlotName','TreatedOG','TreatedMOG')],id.vars = 1)

T_MOG_OG_Plot <- ggplot(T_MOG_OG, aes(x = TreatedPlotName, y = value)) +
                  geom_bar(aes(fill = variable), stat = "identity", color = 'black', position = "dodge") +
                  ggtitle("Treated") +
                  xlab("") +
                  ylab("Number of trees") +
                  ylim(0, 200) +
                  scale_fill_manual(values=c("#4d7358", "#9ed670")) +
                  theme_classic()
                  #scale_color_continuous(guide = guide_legend(override.aes = list(alpha = 0) ) ) +
                  #scale_linetype(guide = guide_legend(override.aes = list(alpha = 0) ) )+
                  #theme(legend.title = element_text(color = "transparent"),
                          #legend.text = element_text(color = "transparent"),
                          #legend.key = element_rect(fill = "transparent"))
    ## Need to find a way to remove the legend but keep same dimensions
    ##https://stackoverflow.com/questions/42438450/make-legend-invisible-but-keep-figure-dimensions-and-margins-the-same

UntreatedPlotName <- subset(est_data, TreatmentStatus == "Untreated")$PlotName
UntreatedMOG <- subset(est_data, TreatmentStatus == "Untreated")$MOG_count
UntreatedOG <- subset(est_data, TreatmentStatus == "Untreated")$OG_count

Untreated_MOG_OG <- data.frame(UntreatedPlotName, UntreatedMOG, UntreatedOG)

UnT_MOG_OG <- melt(Untreated_MOG_OG[,c('UntreatedPlotName','UntreatedOG','UntreatedMOG')],id.vars = 1)

UnT_MOG_OG_Plot <- ggplot(UnT_MOG_OG, aes(x = UntreatedPlotName, y = value)) +
                    geom_bar(aes(fill = variable), stat = "identity", color = 'black', position = "dodge") +
                    ggtitle("Untreated") +
                    xlab("") +
                    ylab("Number of trees") +
                    ylim(0, 200) +
                    scale_fill_manual(values=c("#4d7358", "#9ed670"), labels = c('Old Growth', 'Mature Old Growth')) +
                    theme_classic() +
                    theme(legend.title = element_blank())

grid.arrange(T_MOG_OG_Plot, UnT_MOG_OG_Plot, ncol=1)

## Same as above but in one graph, not two

OG_MOG <- melt(est_data[,c('PlotName','OG_count','MOG_count')],id.vars = 1)

## Can you facet_wrap or facet_grid here???

ggplot(OG_MOG , aes(x = PlotName, y = value)) + 
  geom_bar(aes(fill = variable), stat = "identity", color = 'black', position = "dodge") +
  xlab("Plot") +
  ylab("Number of trees") +
  ylim(0, 200) +
  scale_fill_manual(values=c("#4d7358", "#9ed670"), labels = c('Old Growth', 'Mature Old Growth')) +
  theme_classic() +
  theme(legend.title = element_blank())

## OG and MOG by plot but its a stacked bar graph; I don't like it tbh

MOGMinusOG <- est_data$MOG_count - est_data$OG_count

est_data$MOGMinusOG <- MOGMinusOG

OGvsMOGpart2 <- melt(est_data[,c('PlotName','OG_count','MOGMinusOG')],id.vars = 1)

ggplot(OGvsMOGpart2, aes(x = PlotName, y = value, fill = variable)) + 
  geom_bar(position = 'stack', stat = "identity", color = "black") +
  xlab("Plot") +
  ylab("Number of trees") +
  ylim(0, 300) +
  scale_fill_manual(values=c("#4d7358", "#9ed670"), labels = c('Mature Old Growth', 'Old Growth')) +
  theme_classic() +
  theme(legend.title = element_blank())

## MOG and TPH stacked bar graph

MOGperHectare <- est_data$MOG_count / est_data$PlotSize

est_data$MOGperha <- MOGperHectare 

est_data$NotMOGperha <- est_data$TPH - est_data$MOGperha

MOGPHvsTPH<- melt(est_data[,c('PlotName','MOGperha','NotMOGperha')],id.vars = 1)

MOGPHvsTPH$variable <- factor(MOGPHvsTPH$variable, levels=c("NotMOGperha", "MOGperha"))

MOGPHvsTPH$TreatmentStatus <- est_data$TreatmentStatus

ggplot(MOGPHvsTPH, aes(x = PlotName, y = value, fill = variable)) + 
  geom_bar(position = "fill", stat = "identity", color = "black") +
  xlab("") +
  ylab("Trees per ha") +
  scale_fill_manual(values=c("#005b96", "#6497b1"), labels=c("All other trees", "MOG")) +
  theme_classic() +
  theme(legend.title = element_blank())


-------------------------------------------------------------------------------------------------------
## Plot-level Histograms & Other fun things Carolina is playing around with
#colors= c("#4EDFC7","#C21E56", "#FFC0CB","#95658B", "#FFD700", "#2E8B57", "#89CFF0","#9F2B68","grey", "#5D3FD3", "#CC5500", "#E3963E", "#AFE1AF", "grey","grey","grey")
  #all trees by plot (species-color-coded)
# Change the width of bins
  merged_plots$color <- as.character(merged_plots$color)
ggplot(merged_plots, aes(x=DBH,fill=merged_plots$color)) +
  geom_histogram(binwidth=4)+
  #scale_fill_manual(values = colors)+
  #scale_fill_manual(values=c("#4EDFC7","#C21E56", "#FFC0CB","#95658B", "#FFD700", "#2E8B57", "#89CFF0","#9F2B68","grey", "#5D3FD3", "#CC5500", "#E3963E", "#AFE1AF", "grey","grey","grey"))+
  #scale_color_manual(values = c("grey40","grey40","grey40","grey40" ,"grey40","grey40","grey40","grey40","grey40","grey40", "grey40"))+
  scale_fill_identity()+
  xlab("DBH (cm)")+
  facet_wrap(~TreatmentStatus + PlotName + PlotSize, ncol = 5 )+
  theme_minimal()+
  theme(strip.text = element_text(size =8))

#same but facet by treatment status
# Change the width of bins
  ggplot(merged_plots, aes(x=DBH, fill = color)) +
  geom_histogram(binwidth=2)+
  scale_fill_identity()+
  xlab("DBH (cm)")+
  facet_wrap(~TreatmentStatus)+
    theme_minimal()+
  theme(strip.text = element_text(size =15))
  


merged_plots$Condition <- as.factor(merged_plots$Condition)
#rename condition codes with words that make sense
merged_plots <- merged_plots %>%
  mutate(Cond = case_when(Condition == 1 ~ "Live", 
                          Condition == 2 | Condition == 5 | Condition == 6 ~ "Dead",
                          Condition == 3 | Condition == 4 | Condition == 7 ~ "DALB",
                          Condition == 8 | Condition == 9 | Condition == 10 | Condition == 11 ~"Missing"))

#create condition plot faceted by treatment status with nice logical color scheme
merged_plots$Condition <- as.factor(merged_plots$Cond)
#all trees by plot (live vs. dead)
# Change the width of bins
  ggplot(merged_plots, aes(x=DBH, fill = Cond, color = Cond)) + 
  scale_fill_manual(values=c("#e8bb61", "#c7505c", "#8ccfa8", "#56B4E9"))+
    theme(legend.text = element_text(size=15), legend.title =element_blank(), legend.position = c(0.9, 0.9), legend.key.size = unit(0.4, 'in')) +
  scale_color_manual(values = c("grey40","grey40","grey40","grey40"))+
  geom_histogram(binwidth=2)+
  xlab("DBH (cm)")+
  facet_wrap(~TreatmentStatus)+
  theme_minimal()+
  theme(strip.text = element_text(size =15))
  
# num species by plot
  
  ggplot(merged_plots, aes(x=Species, fill = color)) +
    geom_bar(stat = "count" )+
    scale_fill_identity()+
    facet_wrap(~PlotName)+
    theme_minimal()+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5), axis.ticks.x=element_blank())+
    theme(legend.text = element_text(size=15), legend.title =element_blank(), legend.position = c(0.9, 0.9), legend.key.size = unit(0.4, 'in')) 
    
   
  
  #Jonathan check out this code.  It allows us to set a standard color for each species while keeping the legend displaying the species. 
   ggplot(merged_plots, aes(x=Species, fill = Species)) +
      geom_bar(stat = "count" )+
      #scale_fill_identity()+
      scale_fill_manual(values = c("ABCO" = "#4EDFC7", 
                                   "ACGL" = "#C21E56",  
                                   "JUMO" = "#FFC0CB", 
                                   "JUSC" = "#95658B",
                                   "PIED" = "#FFD700",
                                   "PIPO" = "#2E8B57",
                                   "PIST" = "#89CFF0",
                                   "PRVI" = "#9F2B68",
                                   "PSME" = "#5D3FD3",
                                   "QUGA" = "#CC5500",
                                   "QUUN" = "#E3963E",
                                   "SASC" = "#AFE1AF",
                                   "unknown"= "#808080" ,
                                   "NA" = "#808080" ))+
    facet_wrap(~PlotName)+
      theme_minimal()+
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Stacked percent bar graph of number of species
   
ggplot(merged_plots, aes(x=Species, fill = Species)) +
 geom_bar(stat = "count")+
 #scale_fill_identity()+
 scale_fill_manual(values = c("ABCO" = "#4EDFC7", 
                              "ACGL" = "#C21E56",  
                              "JUMO" = "#FFC0CB", 
                              "JUSC" = "#95658B",
                              "PIED" = "#FFD700",
                              "PIPO" = "#2E8B57",
                              "PIST" = "#89CFF0",
                              "PRVI" = "#9F2B68",
                              "PSME" = "#5D3FD3",
                              "QUGA" = "#CC5500",
                              "QUUN" = "#E3963E",
                              "SASC" = "#AFE1AF",
                              "unknown"= "#808080" ,
                              "NA" = "#808080" ))+
 facet_wrap(~PlotName)+
 xlab("") +
 theme_minimal()+
 theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
