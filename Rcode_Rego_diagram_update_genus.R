#Rego diagram

#Readin raw data
siz <- read.csv2("WF_Bivalve_Body_Size.csv")

#read in the range information
rng <- read.csv2("HP_Bivalve_Ranges_gn.csv", header = T, check.names = F)
row.names(rng) <- rng$Name
rng$Name <- NULL

y <- c("Lo3", "Lo4", "Lo5", "In1", "In2", "Ol1", "Ol2", "Ol3")

df1 <- data.frame(Com = c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1", "In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), 
                  Ass = NA, Ext = NA, Wit = NA, Ori = NA, Num = 0)

tgg <- read.csv2("tgg.csv")

for (i in 1:7) {
  print(paste("Iteration", i))
  
  low <- y[[i]]
  upp <- y[[i+1]]

#subset the interval for comparison
int <- tgg[tgg$StrSeq == low | tgg$StrSeq == upp,]
print(summary(tgg$status))
df1[i,"Num"] <- nrow(tgg)

#add the status of each taxon to the size dataset. You end up with 0,1,2,3,4, where 0 is extinct in the first interval
x <- unique(int$Genus)
df1[i,"Div"] <- length(x)

for (k in 1: length(x)){
  
  for (j in 1: nrow(int)){
    
    if(int[j,"Genus"] == x[[k]]){
    int[j,"Lstatus"] <- (rng[x[[k]], low])
    int[j,"Ustatus"] <- (rng[x[[k]], upp])
    }
  }
}

all_low <- int[int$StrSeq == low,]
all_upp <- int[int$StrSeq == upp,]                 

sur_low <- int[int$Lstatus == 1 | int$Lstatus == 2,]
sur_low <- sur_low[!is.na(sur_low$LN), ]
sur_upp <- int[int$Ustatus == 1 | int$Ustatus == 3,]
sur_upp <- sur_upp[!is.na(sur_upp$LN), ]

sur_low <- sur_low[sur_low$StrSeq == low,]
sur_low <- sur_low[!is.na(sur_low$LN), ]
sur_upp <- sur_upp[sur_upp$StrSeq == upp,]
sur_upp <- sur_upp[!is.na(sur_upp$LN), ]

df1[i,"Ext"] <- mean(sur_low$LN)-mean(all_low$LN)
df1[i,"Wit"] <- mean(sur_upp$LN)-mean(sur_low$LN)
df1[i,"Ori"] <- mean(all_upp$LN)-mean(sur_upp$LN)
df1[i,"Ass"] <- mean(all_upp$LN)-mean(all_low$LN)

}

row.names(df1) <- df1[,"Com"]
df1[,"Com"] <- NULL

summary(df1)

##Plot

rego_col <- c("#75CFE0", "#245726", "#9CE551")
T.t <- t(as.matrix(df1[,2:4])) #trans

pdf("Fig.3b.pdf",12, 9)
par(mar=c(15,5,2,2))
barplot(T.t, ylab="effect on size change [log(mm2)]", xlab="", beside=T, col=rego_col, ylim=c(-0.75, 0.75), border = NA, cex.axis = 1.5, cex.lab=1.5,
        names.arg=c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1","In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), las=1, cex.names = 1.4)

rect(8.5, -2, 12.5, 3.1, col="#fff2f2", border=NA) #extinction 14.5
#text(10.5, -1.8, paste("EH (P/Tr)"), cex=1.8, col="#450303")

rect(12.6, -2, 16.5, 3.1, col="#f2f4ff", border=NA) #P-Tr boundary 22.5
#text(14.5, -1.8, paste("Late Gries."), cex=1.8, col="#030d4d")

rect(24.6, -2, 20.5, 3.1, col="#f2f4ff", border=NA) #P-Tr boundary 22.5
#text(22.5, -1.8, paste("E. Spath."), cex=1.8, col="#030d4d")

rect(xleft=c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5,26.5)-1.75, ybottom=0, 
     xright=c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5, 26.5)+1.75, ytop=df1$Ass, 
     col="grey80", border = NA)

barplot(T.t, ylab="", xlab="", beside=T, col=rego_col, border=NA, add=T, cex.axis = 1.5, cex.lab=1.5,
        names.arg=c("Lo3-Lo4", "Lo4-Lo5", "Lo5-In1","In1-In2", "In2-Ol1", "Ol1-Ol2", "Ol2-Ol3"), las=1, cex.names = 1.4)

abline(h=0, lwd=0.5)

#legend("topright", legend=c("Assemblage size shift", "Disappearance", "Within-lineage", "Appearance"), 
       #bty="n", fill=c("grey80", rego_col), border=c("grey80", rego_col), cex=1.5)

dev.off()
