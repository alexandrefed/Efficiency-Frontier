#chooseCRANmirror()
rm(list=ls())  # To clear all
graphics.off() # To close all graphics
library(MASS)
library(Benchmarking)
# library(maxLik)
library(readxl)
library(ggplot2)
library(stringr)

apparts <- read_excel("Appart_dijon_2018.xlsx")
View(apparts)
str(apparts)
apparts$Parking <- as.factor(apparts$Parking)
apparts$Outdoor_terrace_balcony_garden <- as.factor(apparts$Outdoor_terrace_balcony_garden)
apparts$Parking <- as.factor(apparts$Parking)
apparts$Energy_label <- as.factor(apparts$Energy_label)
summary(apparts$Energy_label)


#A=1 B=2...
#A_label <- apparts[which(apparts$Energy_label == "A")),]
#B_label <- apparts[which(apparts$Energy_label == "B")),]
#C_label <- apparts[which(apparts$Energy_label == "C")),]
#D_label <- apparts[which(apparts$Energy_label == "D")),]
#E_label <- apparts[which(apparts$Energy_label == "E")),]
#F_label <- apparts[which(apparts$Energy_label == "F")),]
#G_label <- apparts[which(apparts$Energy_label == "G")),]
#Giveanumber <- function(Energy_label) {
#  Energy_label <- as.character(Energy_label)
#  if (length(grep("A",Energy_label)) > 0) {
#    return(1)
#  } else if (length(grep("B",Energy_label)) > 0) {
#    return(2)
#  } else if (length(grep("C",Energy_label)) > 0) {
#    return(3)
#  } else if (length(grep("E",Energy_label)) > 0) {
#    return(4)
#  } else if (length(grep("F",Energy_label)) > 0) {
#    return(5)
#  } else if (length(grep("G",Energy_label)) > 0) {
#    return(6)
#  } else {
#    return(0)
#  }
#}
#Energy_label_number <- NULL
#for (i in 1:nrow(apparts)) {
#  Energy_label_number <- c(Energy_label_number, Giveanumber(apparts[i,"Energy_label"]))
#}
#apparts$Energy_label_number <- as.factor(Energy_label_number)
#summary(apparts$Energy_label_number)


# Inputs and output maxtrix
n=length(apparts$`ln y`)
vec <- c(apparts$`ln x1`,apparts$`ln x2`)
X<- matrix(vec,n, ncol=2)
Y=matrix(apparts$`ln y`,n,1) 
#W <- matrix(vec1,n, ncol=5)
p=ncol(X);
q=ncol(Y);

## 1 - fIRST STAGE / technical efficiency  AND  allocative efficiency WITH benchmarking package
# CRS Model
ETEcrs <- dea(X, Y, RTS="crs",ORIENTATION="in", TRANSPOSE=FALSE, SLACK=TRUE)
TEcrs=eff(ETEcrs)
summary(TEcrs) #  descriptive Statistics
dea.plot(X,Y,RTS="crs",ORIENTATION="in-out",txt=rownames(X),lty="dotted",main="CRS")

# VRS Model
ETEvrs <- dea(X, Y, RTS="vrs",ORIENTATION="in", TRANSPOSE=FALSE, SLACK=TRUE)
TEvrs=eff(ETEvrs)
summary(TEvrs)
dea.plot(X,Y,RTS="vrs",ORIENTATION="in-out",txt=rownames(X),main="VRS")

### Descriptive analysis
boxplot(TEcrs, data=apparts, xlab="Flat characteristics", ylab="Prices", col= c("pink", "orange", "lightblue", "skyblue2", "slateblue2"))
boxplot(TEvrs, data=apparts, xlab="Flat characteristics", ylab="Prices", col= c("pink", "orange", "lightblue", "skyblue2", "slateblue2"))

# Compute technical efficiy with some return to scale assumptionVRS: DRS and IRS
ETEdrs <- dea(X=X, Y, RTS="drs",ORIENTATION="in", TRANSPOSE=FALSE)
TEdrs=eff(ETEdrs)
dea.plot(X,Y,RTS="drs",ORIENTATION="in-out",txt=rownames(X),lty="dashed",lwd=2,main="DRS")

ETEirs <- dea(X=X, Y, RTS="irs",ORIENTATION="in", TRANSPOSE=FALSE)
TEirs=eff(ETEirs)
dea.plot(X,Y,RTS="irs",ORIENTATION="in-out",main="IRS")

# Scale efficiency 

SE=eff(ETEcrs)/eff(ETEvrs)


### Bootstrap return to scale
# The bootstrap and calculate CRS and VRS under the assumption that
  # the true technology is CRS (the null hypothesis) and such that the
  # results correponds to the case where CRS and VRS are calculated for
  # the same reference set of firms; to make this happen we set the
  # random number generator to the same state before the calls.
 #To get the an initial value for the random number generating process
  # we save its state (seed)
#S <- round(sum(ETEcrs)/sum(ETEvrs), 4)
S <- round(sum(TEcrs)/sum(TEvrs), 4)
save.seed <- sample.int(1e9,1)
set.seed(save.seed)
bc <- dea.boot(X=X,Y, NREP=1000,ORIENTATION="in", RTS="crs")
  set.seed(save.seed)
bv <- dea.boot(X=X,Y, NREP=1000, ORIENTATION="in", RTS="vrs")
  # Calculate the statistic for each bootstrap replica
  
bs <- round(colSums(bc$boot)/colSums(bv$boot), 4)
  # The critical value for the test (default size \code{alpha} of test is 5%)
  critValue(bs, alpha=.1)
S
  # Accept the hypothesis at 5% level?
  critValue(bs, alpha=.1) <= S
  # The probability of observering a smaller value of S when the
  # hypothesis is true; the p--value.
  typeIerror(S, bs)
  # Accept the hypothesis at size level 5%?
  typeIerror(S, bs) >= .1

# Bias correction for Farrell measures cRS

#bc <- dea.boot(X,Y, NREP=2000,ORIENTATION="in", RTS="crs")
TEest = bv$eff
Far_BC = bv$eff.bc
biaiseff = bv$bias
sigeff = sqrt(bv$var)
FarCI = bv$conf.int
c = abs(biaiseff) > (sigeff/4)
cbind(TEest,Far_BC, biaiseff, sigeff,FarCI)

#2-Estimation second stage with Benchmarking
apparts$TEvrs=TEvrs


# analyze by technology nbre de parkings, et autres variables

boxplot(TEvrs~apparts$Energy_label,xlab="TEVRS", ylab="Efficiency score", col= c("pink", "lightblue", "skyblue2", "slateblue2", "blue", "blue", "orange"))
