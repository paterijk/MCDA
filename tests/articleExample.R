library(MCDA)

# load performance table csv file
# provided with the MCDA package

f <- system.file("datasets","performanceTable2.csv",package="MCDA")

pT <- read.csv(file = f, header=TRUE, row.names=1)

###################
# first attempt   #
# filtering rules #
###################

# to filter out cars which do not
# respect Thierry's initial rules

fPT <- pT[(pT$g4>=2 & pT$g5>=2 & pT$g2 < 30), ]

# to drop car a14 from the table

fPT <- fPT[!(rownames(fPT) %in% "a14"), ]

print(fPT)

criteriaMinMax <- c("min","min","min","max","max")

names(criteriaMinMax) <- colnames(pT)

plotRadarPerformanceTable(fPT, criteriaMinMax, overlay=FALSE, bw=TRUE, lwd =5)

##################################
# second attempt                 #
# normalization and weighted sum #
##################################

# 5 barplots 
par(mfrow=c(2,3))
for (i in 1:dim(pT)[2]){
  yaxis <- range(pT[,i])*c(0.99,1.05)
  if (criteriaMinMax[i] =="min")
    oPT <- pT[order(pT[,i],decreasing=FALSE),]
  else
    oPT <- pT[order(pT[,i],decreasing=TRUE),]
  name <-paste(colnames(pT)[i]," (",criteriaMinMax[i],")", sep="")
  barplot(oPT[,i], main=name, names.arg = rownames(oPT), density = i*10, ylim = yaxis, xpd=FALSE)  
}

# normalization of the data from the performance table

normalizationTypes <- c("percentageOfMax","percentageOfMax",
                        "percentageOfMax","percentageOfMax", "percentageOfMax")

names(normalizationTypes) <- c("g1","g2","g3","g4","g5")

nPT <- normalizePerformanceTable(pT,normalizationTypes)

# weighted sum

w <- c(-1,-2,-1,0.5,0.5)
names(w) <- colnames(pT)
ws<-weightedSum(nPT,w)

# rank the scores of the alternatives
rank(-ws)

# add supplementary car to pT

missing <- c(16966,30,37.7,2.33,3.25)
pT<-rbind(pT,missing)
rownames(pT)[14] <- "a10"

# normalization

nPT <- normalizePerformanceTable(pT,normalizationTypes)

# weighted sum

ws<-weightedSum(nPT,w)

# rank the scores of the alternatives
rank(-ws)

###################
# third attempt   #
# MAVT            #
###################

# ranks of the alternatives

alternativesRanks <- c(1,2,3,4,5)
names(alternativesRanks) <- c("a11","a03","a13","a09","a14")

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(2,2,2,2,2)
names(criteriaNumberOfBreakPoints) <- colnames(pT)

# lower bounds of the criteria for the determination of value functions

criteriaLBs=apply(pT,2,min)
names(criteriaLBs) <- colnames(pT)

# upper bounds of the criteria for the determination of value functions

criteriaUBs=apply(pT,2,max)
names(criteriaUBs) <- colnames(pT)

# the separation threshold

epsilon <-0.01

x<-UTA(pT, criteriaMinMax, 
       criteriaNumberOfBreakPoints, epsilon, 
       alternativesRanks = alternativesRanks,
       criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)

# plot the piecewise linear value functions

plotPiecewiseLinearValueFunctions(x$valueFunctions)

# ranks of the alternatives for the second try

alternativesRanks <- c(1,2,3,4,5,6,7)
names(alternativesRanks) <- c("a11","a03","a08","a04","a13","a09","a14")

x2<-UTA(pT, criteriaMinMax, 
        criteriaNumberOfBreakPoints, epsilon, 
        alternativesRanks = alternativesRanks,
        criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)

# plot the piecewise linear value functions

plotPiecewiseLinearValueFunctions(x2$valueFunctions)

# apply the value functions on the original performance table

tPT <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(x2$valueFunctions, pT)

# calculate the overall score of each alternative

mavt <- weightedSum(tPT,rep(1,5))

# plot the ranking of the cars

plotAlternativesValuesPreorder(mavt, decreasing=TRUE)
