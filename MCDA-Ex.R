pkgname <- "MCDA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MCDA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("UTA")
### * UTA

flush(stderr()); flush(stdout())

### Name: UTA
### Title: UTA method to elicit value functions.
### Aliases: UTA
### Keywords: methods

### ** Examples

# the separation threshold

epsilon <-0.05

# the performance table

performanceTable <- rbind(
  		c(3,10,1),
			c(4,20,2),
			c(2,20,0),
			c(6,40,0),
			c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# ranks of the alternatives

alternativesRanks <- c(1,2,2,3,4)

names(alternativesRanks) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(3,4,4)

names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)

x<-UTA(performanceTable, alternativesRanks, criteriaMinMax, 
        criteriaNumberOfBreakPoints, epsilon)

# plot the value functions obtained

plotValueFunctions(x$valueFunctions)

# apply the value functions on the original performance table

transformedPerformanceTable <- applyValueFunctionsOnPerformanceTable(
  x$valueFunctions, 
  performanceTable, 
  criteriaMinMax)

# calculate the overall score of each alternative

weightedSum(transformedPerformanceTable,c(1,1,1))

# ----------------------------------------
# ranking some cars (from the original article on UTA by Siskos and Lagreze, 1982)

# the separation threshold

epsilon <-0.01

# the performance table

performanceTable <- rbind(      
c(173, 11.4, 10.01, 10, 7.88, 49500),
c(176, 12.3, 10.48, 11, 7.96, 46700),
c(142, 8.2, 7.30, 5, 5.65, 32100),
c(148, 10.5, 9.61, 7, 6.15, 39150), 
c(178, 14.5, 11.05, 13, 8.06, 64700), 
c(180, 13.6, 10.40, 13, 8.47, 75700),
c(182, 12.7, 12.26, 11, 7.81, 68593), 
c(145, 14.3, 12.95, 11, 8.38, 55000),
c(161, 8.6, 8.42, 7, 5.11, 35200), 
c(117, 7.2, 6.75, 3, 5.81, 24800)
)

rownames(performanceTable) <- c(
  "Peugeot 505 GR",
  "Opel Record 2000 LS",
  "Citroen Visa Super E",
  "VW Golf 1300 GLS",
  "Citroen CX 2400 Pallas",
  "Mercedes 230",
  "BMW 520",
  "Volvo 244 DL",
  "Peugeot 104 ZS",
  "Citroen Dyane")

colnames(performanceTable) <- c(
  "MaximalSpeed",
  "ConsumptionTown",
  "Consumption120kmh",
  "HP",
  "Space",
  "Price")

# ranks of the alternatives

alternativesRanks <- c(1,2,3,4,5,6,7,8,9,10)

names(alternativesRanks) <- row.names(performanceTable)

# criteria to minimize or maximize

criteriaMinMax <- c("max","min","min","max","max","min")

names(criteriaMinMax) <- colnames(performanceTable)

# number of break points for each criterion

criteriaNumberOfBreakPoints <- c(5,4,4,5,4,5)

names(criteriaNumberOfBreakPoints) <- colnames(performanceTable)

# lower bounds of the criteria to be used for the determination of value functions

criteriaLBs=c(110,7,6,3,5,20000)

names(criteriaLBs) <- colnames(performanceTable)

# upper bounds of the criteria to be used for the determination of value functions

criteriaUBs=c(190,15,13,13,9,80000)

names(criteriaUBs) <- colnames(performanceTable)

x<-UTA(performanceTable, alternativesRanks, criteriaMinMax, 
        criteriaNumberOfBreakPoints, epsilon, 
        criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)
        

# plot the value functions obtained

plotValueFunctions(x$valueFunctions)

# apply the value functions on the original performance table

transformedPerformanceTable <- applyValueFunctionsOnPerformanceTable(
      x$valueFunctions, 
      performanceTable, 
      criteriaMinMax)

# calculate the overall score of each alternative

weights<-c(1,1,1,1,1,1)

names(weights)<-colnames(performanceTable)

weightedSum(transformedPerformanceTable,c(1,1,1,1,1,1))

# ----------------------------------------
# Let us consider only 2 criteria : Price and MaximalSpeed. What happens ? 

x<-UTA(performanceTable, alternativesRanks, criteriaMinMax, 
        criteriaNumberOfBreakPoints, epsilon, 
        criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs,
        criteriaIDs = c("MaximalSpeed","Price"))
        

# plot the value functions obtained

plotValueFunctions(x$valueFunctions, criteriaIDs = c("MaximalSpeed","Price"))

# apply the value functions on the original performance table

transformedPerformanceTable <- applyValueFunctionsOnPerformanceTable(
  x$valueFunctions, 
  performanceTable, 
  criteriaMinMax, 
  criteriaIDs = c("MaximalSpeed","Price")
  )

# calculate the overall score of each alternative

weights<-c(1,1,1,1,1,1)

names(weights)<-colnames(performanceTable)

weightedSum(transformedPerformanceTable,
          weights, criteriaIDs = c("MaximalSpeed","Price"))




cleanEx()
nameEx("applyValueFunctionsOnPerformanceTable")
### * applyValueFunctionsOnPerformanceTable

flush(stderr()); flush(stdout())

### Name: applyValueFunctionsOnPerformanceTable
### Title: Apply value functions on performance table.
### Aliases: applyValueFunctionsOnPerformanceTable
### Keywords: methods

### ** Examples


# the value functions

v<-list(
  Price = array(c(30, 0, 16, 0, 2, 0.0875), 
    dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
  Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
  Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))

# the performance table

performanceTable <- rbind(
    	c(3,10,1),
			c(4,20,2),
			c(2,20,0),
			c(6,40,0),
			c(30,30,3))

rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")

colnames(performanceTable) <- c("Price","Time","Comfort")

# criteria to be minimized or maximized

criteriaMinMax <- c("min","min","max")

names(criteriaMinMax) <- colnames(performanceTable)

# the transformed performance table

applyValueFunctionsOnPerformanceTable(v,performanceTable, criteriaMinMax)



cleanEx()
nameEx("plotValueFunctions")
### * plotValueFunctions

flush(stderr()); flush(stdout())

### Name: plotValueFunctions
### Title: Function to plot value functions.
### Aliases: plotValueFunctions
### Keywords: methods

### ** Examples


v<-list(
  Price = array(c(30, 0, 16, 0, 2, 0.0875), 
    dim=c(2,3), dimnames = list(c("x", "y"), NULL)), 
  Time = array(c(40, 0, 30, 0, 20, 0.025, 10, 0.9), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)), 
  Comfort = array(c(0, 0, 1, 0, 2, 0.0125, 3, 0.0125), 
    dim = c(2, 4), dimnames = list(c("x", "y"), NULL)))

# plot the value functions

plotValueFunctions(v)



cleanEx()
nameEx("weightedSum")
### * weightedSum

flush(stderr()); flush(stdout())

### Name: weightedSum
### Title: Weighted sum of evaluations of alternatives.
### Aliases: weightedSum
### Keywords: methods

### ** Examples

performanceTable <- matrix(runif(3*4), ncol=3)

row.names(performanceTable) <- c("x1","x2","x3","x4")

colnames(performanceTable) <- c("g1","g2","g3")

weights <- c(1,2,3)

names(weights) <- c("g1","g2","g3")

overall1 <- weightedSum(performanceTable, weights)

overall2 <- weightedSum(performanceTable, weights, 
      alternativesIDs <- c("x2","x3"), criteriaIDs <- c("g2","g3"))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
