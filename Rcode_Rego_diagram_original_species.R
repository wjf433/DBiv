library("ggplot2")

#Rego diagram, where the relative abundance of different species affects the results
#This code is for Fig. 4 and Fig. S12 

#Read in raw data
siz <- read.csv2("WF_Bivalve_Body_Size.csv")

#read in the range information
rng <- read.csv2("HP_Bivalve_Ranges.csv", header = T, check.names = F) # 00absent, 1=range-through, 2=originates, 3 =extinct, 4=singleton
row.names(rng) <- rng$Name #change row names
rng$Name <- NULL # remove redundant column

#Set the different time bins
y <- c("Lo3", "Lo4", "Lo5", "In1", "In2", "Ol1", "Ol2", "Ol3")

#create a data frame that will store the plotting data
#you will need to set the time bin comparison names
df1 <- data.frame(Com = c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1", "In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), 
                  Ass = NA, Ext = NA, Wit = NA, Ori = NA, Num = 0)

#For plotting Fig.4, you set the iteration at i<-3 for the mass extinction and bypass the initial for-loop command.
#i<- 3

#Main loop to calculate mean body size changes
for (i in 1: nrow(df1)) {
  print(paste("Iteration", i))

  low <- y[[i]] # set the lower time bin name
  upp <- y[[i+1]] # set the upper time bin name

#subset the interval for comparison
int <- siz[siz$StrSeq == low | siz$StrSeq == upp,]
print(summary(int$status))
df1[i,"Num"] <- nrow(int)

#add the status of each taxon to the size dataset (0,1,2,3,4)
x <- unique(int$Name)
df1[i,"Div"] <- length(x)

for (k in 1: length(x)){
  
  for (j in 1: nrow(int)){
    
    if(int[j,"Name"] == x[[k]]){
    int[j,"Lstatus"] <- (rng[x[[k]], low])
    int[j,"Ustatus"] <- (rng[x[[k]], upp])
    }
  }
}

#now create groups of samples based on status from main dataset
#whole assemblage body sizes in the lower time bin
all_low <- int[int$Lstatus == 1 | int$Lstatus == 2| int$Lstatus == 3| int$Lstatus == 4,]
all_low <- all_low[!is.na(all_low$Worker), ] # work around to remove NAs created by R

#lower time bin survivors
sur_low <- int[int$Lstatus == 1 | int$Lstatus == 2,]
sur_low <- sur_low[!is.na(sur_low$Worker), ] # work around to remove NAs created by R

#upper time bin survivors
sur_upp <- int[int$Ustatus == 1 | int$Ustatus == 3,]
sur_upp <- sur_upp[!is.na(sur_upp$Worker), ] # work around to remove NAs created by R

#whole assemblage of body sizes in the upper time bin
all_upp <- int[int$Ustatus == 1 | int$Ustatus == 2| int$Ustatus == 3| int$Ustatus == 4,]
all_upp <- all_upp[!is.na(all_upp$Worker), ] # work around to remove NAs created by R

sur_low <- sur_low[sur_low$StrSeq == low,]
sur_upp <- sur_upp[sur_upp$StrSeq == upp,]

#calculate the different means for the Rego categories
df1[i,"Ext"] <- mean(sur_low$LN)-mean(all_low$LN)
df1[i,"Wit"] <- mean(sur_upp$LN)-mean(sur_low$LN)
df1[i,"Ori"] <- mean(all_upp$LN)-mean(sur_upp$LN)
df1[i,"Ass"] <- mean(all_upp$LN)-mean(all_low$LN)

#for new plot
ext <- int[int$Lstatus == 3 | int$Lstatus == 4,]
ext <- ext$LN
ori <- int[int$Ustatus == 2 | int$Ustatus == 4,]
ori <- ori$LN
slow <- sur_low$LN
supp <- sur_upp$LN

df2 <- data.frame(length = 1:(length(ext)+length(supp)+length(slow)+length(ori)), ID = NA, value = NA)

for(i in 1: length(ext)){
  df2[i,"ID"] <- "ext"
  df2[i, "value"] <- ext[[i]]
}
for(i in 1: length(slow)){
  df2[i+length(ext),"ID"] <- "slow"
  df2[i+length(ext), "value"] <- slow[[i]]
}
for(i in 1: length(supp)){
  df2[i+length(ext)+length(slow),"ID"] <- "supp"
  df2[i+length(ext)+length(slow), "value"] <- supp[[i]]
}
for(i in 1: length(ori)){
  df2[i+length(ext)+length(supp)+length(slow),"ID"] <- "ori"
  df2[i+length(ext)+length(supp)+length(slow), "value"] <- ori[[i]]
}


df2$ID <- factor(df2$ID, levels = c("ext", "slow", "supp", "ori"))
ggplot(df2, aes(ID, value, colour = ID))+
  geom_jitter(width = 0.25) +
  geom_boxplot(colour = "darkgrey")+
  scale_color_manual(values=c("#75CFE0", "#245726", "#245726", "#9CE551"))+
  scale_y_continuous(limits = c(0, 3.5))+
  theme_bw() +
  ylab("Geometric Body Size [log(mm2)]") +
  xlab(element_blank())+
  theme(legend.position="none", panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

ggsave("extinction_sizes.eps", plot=last_plot(), device="eps", width = 90, height = 90, units = c("mm"))

###################

}

#rename the rows of the results and remove name column
row.names(df1) <- df1[,"Com"]
df1[,"Com"] <- NULL

#check your results
summary(df1)

##Plot
#set the colours
rego_col <- c("#75CFE0", "#245726", "#9CE551")
T.t <- t(as.matrix(df1[,2:4])) #trans

pdf("S12.pdf",12, 9)
par(mar=c(15,5,2,2))
barplot(T.t, ylab="effect on size change [log(mm2)]", xlab="", beside=T, col=rego_col, ylim=c(-1, 1), border = NA, cex.axis = 1.5, cex.lab=1.5,
        names.arg=c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1","In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), las=1, cex.names = 1.4)

rect(8.5, -1.5, 12.5, 3.1, col="#fff2f2", border=NA) #extinction 14.5
#text(10.5, -1.8, paste("EH (P/Tr)"), cex=1.8, col="#450303")

rect(12.6, -1.5, 16.5, 3.1, col="#f2f4ff", border=NA) #P-Tr boundary 22.5
#text(14.5, -1.8, paste("Late Gries."), cex=1.8, col="#030d4d")

rect(24.6, -1.5, 20.5, 3.1, col="#f2f4ff", border=NA) #P-Tr boundary 22.5
#text(22.5, -1.8, paste("E. Spath."), cex=1.8, col="#030d4d")

text(2.5, -.4, paste(df1[1,"Num"]))
text(6.5, -.4, paste(df1[2,"Num"]))
text(10.5, -.4, paste(df1[3,"Num"]))
text(14.5, -.4, paste(df1[4,"Num"]))
text(18.5, -.4, paste(df1[5,"Num"]))
text(22.5, -.4, paste(df1[6,"Num"]))
text(26.5, -.4, paste(df1[7,"Num"]))

rect(xleft=c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5,26.5)-1.75, ybottom=0, 
     xright=c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5, 26.5)+1.75, ytop=df1$Ass, 
     col="grey80", border = NA)

barplot(T.t, ylab="", xlab="", beside=T, col=rego_col, border=NA, add=T, cex.axis = 1.5, cex.lab=1.5,
        names.arg=c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1","In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), las=1, cex.names = 1.4)

abline(h=0, lwd=0.5)

legend("topright", legend=c("Assemblage size shift", "Disappearance", "Within-lineage", "Appearance"), 
       bty="n", fill=c("grey80", rego_col), border=c("grey80", rego_col), cex=1.5)
dev.off()
