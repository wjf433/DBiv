# -------------------------------------------------------------------------- #
#
# This R code accompanies the article
# Foster, W.J, Prinoth, H., Kustatscher, E. and Hautmann, M. (2025). An unpredictable body size response to the Permo-Triassic climate crisis.
#  
# -------------------------------------------------------------------------- #
#
# Version 18/08/2025
#
# -------------------------------------------------------------------------- #

#Check your working directory, so the code can read in the correct files. If required, change you workig directory using setwd()
getwd()
#setwd()

#Open the packages required to run the analyses and generate the required plots.If you do not already have these packages
#then change"library" to "install.packages" during first use, and then open the packages.
library("ggplot2")
library("plyr")
library("viridis")

#read in the raw data. here I use the commend .csv2, because a German version of excel was used for saving the .csv file.
#If you modified the .csv file, you may need to change the command to "read.csv".
siz <- read.csv2("WF_Bivalve_Body_Size.csv")

#This just selects the different genera that will be included in the analysis. It essentially stops errors appearing.
tow <- siz[siz$Genus=="Acharax" | siz$Genus=="Avichlamys" | siz$Genus=="Bakevellia" | siz$Genus=="Hoernesia" | siz$Genus=="Entolium"| siz$Genus=="Pleuromya"| siz$Genus=="Homomya"| siz$Genus=="Claraia" | siz$Genus=="Costatoria"| siz$Genus=="Crittendenia" | siz$Genus=="Edmondia" | siz$Genus=="Etheripecten" | siz$Genus=="Eumorphotis" | siz$Genus=="Gardenapecten"| siz$Genus=="Grammatodon (Cosmetodon)"| siz$Genus=="Ladinomya" | siz$Genus=="Leptochondria"| siz$Genus=="Lovaralucina"| siz$Genus=="Neoschizodus" | siz$Genus=="Palaeolima"| siz$Genus=="Permophorus" |siz$Genus=="Pernopecten" | siz$Genus=="Promyalina" |siz$Genus=="Promytilus" | siz$Genus=="Pteria" | siz$Genus=="Schizodus" | siz$Genus=="Scythentolium" | siz$Genus=="Stutchburia"| siz$Genus=="Tambanella"| siz$Genus=="Towapteria"| siz$Genus=="Unionites" | siz$Genus=="Vacunella" | siz$Genus=="Volsellina",]

#This organises the stratigraphic sequences into the correct order for the analyses.
tow$StrSeq <- factor(tow$StrSeq, levels = c("Lo3", "Lo4", "Lo5", "In1", "In2", "Ol1", "Ol2", "Ol3"))
#This organises the species into their genera, and also phylogeny.
tow$Name <- factor(tow$Name, levels = c("Vacunella elongata", 
                                          "Stutchburia costata", "Stutchburia sp.", "Stutchburia tschernyschewi",
                                          "Costatoria subrotunda", "Costatoria costata",
                                          "Schizodus obscurus",
                                          "Neoschizodus elongatus", "Neoschizodus laevigatus", "Neoschizodus orbicularis", "Neoschizodus ovatus",
                                          "Ladinomya fosteri",
                                          "Homomya sp.",
                                          "Pleuromya elongata",
                                          "Unionites canalensis", "Unionites fassaensis", "Unionites jacobi", "Unionites sp.", "Unionites sp.2",
                                          "Permophorus bregeri",
                                          "Edmondia hautmanni",
                                          "Lovaralucina covidi", "Lovaralucina truncata",
                                          "Grammatodon obsoletiformis",
                                          "Palaeolima badiotica",
                                          "Promytilus merlai",
                                          "Volsellina carinata",
                                          "Hoernesia sp.",
                                          "Bakevellia albertii", "Bakevellia binneyi", "Bakevellia castelli", "Bakevellia ladina", "Bakevellia costata", "Bakevellia ceratophaga", "Bakevellia exporrecta", "Bakevellia preromangica", "Bakevellia sp.",
                                          "Towapteria peracuta", "Towapteria scythica",
                                          "Pteria ussurica",
                                          "Tambanella stetteneckensis",
                                          "Promyalina schamarae", "Promyalina eduliformis",
                                          "Crittendenia sp.",
                                          "Avichlamys voelseckhofensis",
                                          "Claraia aurita","Claraia sp.", "Claraia clarae",
                                          "Scythentolium sp.", "Scythentolium sp.A", "Scythentolium tirolicum",
                                          "Entolium discites",
                                          "Pernopecten latangulatus", "Pernopecten tirolensis",
                                          "Etheripecten stuflesseri",
                                          "Eumorphotis beneckei", "Eumorphotis hinnitidea", "Eumorphotis kittli", "Eumorphotis lorigae", "Eumorphotis multiformis", "Eumorphotis reticulata", "Eumorphotis praecurrens", "Eumorphotis sp.", "Eumorphotis striatocostata", "Eumorphotis telleri", "Eumorphotis tenuistriata",
                                          "Gardenapecten comelicanus", "Gardenapecten trinkeri",
                                          "Leptochondria albertii",
                                          "Acharax frenademezi"))

#This plots the raw data and is a first look at the database.
ggplot(tow, aes(x = StrSeq, y = LN, col = Name)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  theme(legend.position = "bottom", legend.title =element_blank())

#This plots the median size for each genus, as a data exploration exersize.
ggplot(tow, aes(x = StrSeq, y = LN, col = Genus)) + 
  geom_point(stat = "summary", fun = "median") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  ylim(0, 4)

# ----------------------------FIGURE-02------------------------------------ #

#function for calculating the 95% confidence intervals
#function comes from https://www.nagraj.net/notes/summary-se/, accessed 18/08/2025
#you can change mean to median, if you want to change how you measure average body size, but here the trends stay the same with both.
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
    # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
    # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

#Figure 2B
#This plot, plots the mean size for each species and confidence interval.
#This will produce the error NaNs were produced, but that is expected for species with not enough measurements to calculate confidence intervals.
tgc <- summarySE(tow, measurevar="LN", groupvars=c("StrSeq","Name"))
pd <- position_dodge(0.1) # stops points pilling on top of each other.

ggplot(tgc, aes(x=StrSeq, y=LN, colour=Name)) + 
  geom_errorbar(aes(ymin=LN-ci, ymax=LN+ci), width=.1, position = pd) +
  geom_line(group=tgc$Name, position = pd) +
  geom_point(position = pd) +
  xlab("Stratigraphic Sequence") + 
  ylab("Geometric Size (Natural Log)") +
  theme(legend.position="bottom", legend.title = element_blank())+
  ylim(1, 3.5)

#Figure 2A
#This plot, plots the mean size for each genus and confidence interval.
tgg <- summarySE(tow, measurevar="LN", groupvars=c("StrSeq","Genus"))
ggplot(tgg, aes(x=StrSeq, y=LN, colour=Genus)) + 
  geom_errorbar(aes(ymin=LN-ci, ymax=LN+ci), width=.1, position = pd) +
  geom_line(group=tgg$Genus, position = pd) +
  geom_point(position = pd) +
  xlab("Stratigraphic Sequence") + 
  ylab("Geometric Size (Natural Log)") +
  theme(legend.position="bottom", legend.title = element_blank())+
  ylim(1, 3.5)

#The code for the plots in Fig. 3 and the Rego et al. 82012) diagrams require the variables tgc and tgg to be saved as .csv files. 
#write.csv2() is for German computers.
write.csv2(tgc, "tgc.csv")
write.csv2(tgg, "tgg.csv")

# ----------------------------Sig.-tests------------------------------------ #

#Mann-Whitney U tests for every genus to see if there are significant size changes
taxa <- unique(tow$Genus)
for (i in 1: length(taxa)){
  
  print(taxa[[i]])
  
  sbs <- tow[tow$Genus==taxa[[i]],]
  print(pairwise.wilcox.test(sbs$LN, sbs$StrSeq, p.adjust.method="bonferroni"))
  
}

#Mann-Whitney U tests for every species to see if there are significant size changes
taxa <- unique(tow$Name)
for (i in 1: length(taxa)){
  
  print(taxa[[i]])
  
  sbs <- tow[tow$Name==taxa[[i]],]
  print(pairwise.wilcox.test(sbs$LN, sbs$StrSeq, p.adjust.method="bonferroni"))
  
}

# ----------------------------FIGURE-05------------------------------------ #

#species comparisons within a genus
#For the genus Unionites
unio <- tow[tow$Genus=="Unionites",]
ggplot(unio, aes(x = StrSeq, y = LN, col = Species)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=3.5))+
  ylim(0, 4) +
  labs(y = "Geometric Size (log(mm))", x = "Sequence Stratigraphic Framework")

#For the genus Eumorphotis
eumo <- tow[tow$Genus=="Eumorphotis",]
ggplot(eumo, aes(x = StrSeq, y = LN, col = Species)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  geom_vline(aes(xintercept=3.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  ylim(0, 5)+
  labs(y = "Geometric Size (log(mm))", x = "Sequence Stratigraphic Framework")

#For the family Bakevellidae
bak <- tow[tow$Family=="BAKEVELLIIDAE",]
ggplot(bak, aes(x = StrSeq, y = LN, col = Name)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  geom_vline(aes(xintercept=3.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  ylim(0, 5) +
  labs(y = "Geometric Size (log(mm))", x = "Sequence Stratigraphic Framework")

#For the genus Pernopecten
per <- tow[tow$Genus=="Pernopecten",]
ggplot(per, aes(x = StrSeq, y = LN, col = Species)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  geom_vline(aes(xintercept=3.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  ylim(0, 5) +
  labs(y = "Geometric Size (log(mm))", x = "Sequence Stratigraphic Framework")

#For the related genera Schizodus and Neoschizodus
sch <- tow[tow$Genus=="Schizodus" | siz$Genus=="Neoschizodus",]
ggplot(sch, aes(x = StrSeq, y = LN, col = Name)) + 
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  theme_bw() +
  geom_vline(aes(xintercept=2.5))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  geom_hline(aes(yintercept=0)) +
  ylim(0, 5) +
  labs(y = "Geometric Size (log(mm))", x = "Sequence Stratigraphic Framework")