#------------ Init ----------#

# load the library

library(MCDA)

# set up criteria parameters

criteria <- c("c1","c2","c3","c4","c5")

criteriaMinMax <- c("max","max","max","max","max")

names(criteriaMinMax) <- criteria

# set up categories parameters

categories <- c("Good","Neutral","Bad")

categoriesRanks <- c(1,2,3)

names(categoriesRanks) <- categories

# load performance table

f <- system.file("datasets", "dataFLOSS.csv", package = "MCDA")

pT <- read.csv(f, TRUE, ",", "\"", ".")

pT <- data.matrix(pT)

rownames(pT) <- 1:dim(pT)[1]

colnames(pT) <- criteria

#------------ First iteration ----------#

# get assignments of first 25 alternatives

assig <-c("Bad", "Neutral", "Bad", "Bad", "Neutral","Good",
          "Neutral", "Good", "Bad", "Bad", "Good", "Neutral",
          "Bad", "Bad", "Bad", "Bad", "Bad", "Neutral",
          "Bad", "Neutral", "Bad", "Bad", "Neutral", "Bad",
          "Bad")

names(assignments) <- 1:25

# generate MR-Sort model

m1 <- MRSortInferenceExact(pT, assig, categoriesRanks, 
                          criteriaMinMax, veto = FALSE,
                          readableWeights = TRUE, 
                          readableProfiles = TRUE,
                          solver = "cplex",
                          alternativesIDs = names(assig))

# model does not fit the assignment examples

m1$humanReadableStatus

# generate incompatible assignments

incomp <- MRSortIdentifyIncompatibleAssignments(pT, assig,
                                                categoriesRanks,
                                                criteriaMinMax,
                                                veto = FALSE,
                                                solver = "cplex",
                                                alternativesIDs = names(assig))

# generate alternative assignments for each set of incompatible assignments

for(incAlt in incomp$incompatibleSets)
{
  m1pr <- MRSortInferenceExact(pT, assig, categoriesRanks,
                               criteriaMinMax, veto = FALSE,
                               readableWeights = TRUE,
                               readableProfiles = TRUE,
                               alternativesIDs = names(assign)[!names(assig) %in% incAlt],
                               solver = "cplex")
  
  MRSort(pT, m1pr$profilesPerformances, categoriesRanks, m1pr$weights, criteriaMinMax,
         m1pr$lambda, alternativesIDs = incAlt)
  
}

# DM expresses posibility to update alternative 2 but not the rest -> trying MR-Sort with vetoes

m1 <- MRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                           veto = TRUE, readableWeights = TRUE, 
                           readableProfiles = TRUE, solver = "cplex",
                           alternativesIDs = names(assig))

# model fits

m1$humanReadableStatus

# plot the model

criteriaLBs=c(-2,-2,-2,-2,-2)

names(criteriaLBs) <- criteria

criteriaUBs=c(2,2,2,2,2)

names(criteriaUBs) <- criteria

plotMRSortSortingProblem(pT, m1$profilesPerformances, categoriesRanks, assig,
                         criteriaMinMax, criteriaUBs, criteriaLBs, 
                         NULL, m1$vetoPerformances, 'V',
                         m1$weights, m1$lambda,
                         alternativesIDs = "no alternatives")

#------------ Second iteration ----------#

# add assignments for the next 10 alternatives

assig <-c(assig, "Bad", "Bad", "Neutral", "Bad", "Bad",
          "Bad", "Bad", "Neutral", "Good", "Neutral")

names(assignments) <- 1:35

# generate MR-Sort model with vetoes

m2 <- MRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                           veto = TRUE, readableWeights = TRUE, 
                           readableProfiles = TRUE, solver = "cplex",
                           alternativesIDs = names(assig))

# model does not fit

m2$humanReadableStatus

# generate incompatible assignments

incomp <- MRSortIdentifyIncompatibleAssignments(pT, assig, categoriesRanks,
                                                criteriaMinMax, veto = TRUE,
                                                solver = "cplex",
                                                alternativesIDs = names(assig))

# generate alternative assignments for each set of incompatible assignments

for(incAlt in incomp$incompatibleSets)
{
  m2pr <- MRSortInferenceExact(pT, assig, categoriesRanks,
                               criteriaMinMax, veto = FALSE,
                               readableWeights = TRUE,
                               readableProfiles = TRUE,
                               alternativesIDs = names(assign)[!names(assig) %in% incAlt],
                               solver = "cplex")
  
  MRSort(pT, m2pr$profilesPerformances, categoriesRanks, m2pr$weights, criteriaMinMax,
         m2pr$lambda, alternativesIDs = incAlt)
  
}

# DM updates assignments

assig[2] <- "Bad"

assig[35] <- "Bad"

# generate updated model

m2 <- MRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                           veto = TRUE, readableWeights = TRUE, 
                           readableProfiles = TRUE, solver = "cplex",
                           alternativesIDs = names(assig))

#------------ Third iteration ----------#

# add assignments for the next 10 alternatives

assig <-c(assig, "Bad", "Neutral", "Bad", "Neutral",
          "Good", "Bad", "Bad", "Good", "Bad", "Bad")

names(assignments) <- 1:45

# generate MR-Sort model with vetoes

m3 <- MRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                           veto = TRUE, readableWeights = TRUE, 
                           readableProfiles = TRUE, solver = "cplex",
                           alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# generate incompatible assignments

incomp <- MRSortIdentifyIncompatibleAssignments(pT, assig, categoriesRanks,
                                                criteriaMinMax, veto = TRUE,
                                                solver = "cplex",
                                                alternativesIDs = names(assig))

# generate alternative assignments for each set of incompatible assignments

for(incAlt in incomp$incompatibleSets)
{
  m3pr <- MRSortInferenceExact(pT, assig, categoriesRanks,
                               criteriaMinMax, veto = FALSE,
                               readableWeights = TRUE,
                               readableProfiles = TRUE,
                               alternativesIDs = names(assign)[!names(assig) %in% incAlt],
                               solver = "cplex")
  
  MRSort(pT, m3pr$profilesPerformances, categoriesRanks, m3pr$weights, criteriaMinMax,
         m3pr$lambda, alternativesIDs = incAlt)
  
}

# DM does not update any assignemnts

# generate other MR-Sort variants

# MR-Sort with dictators

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "D", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# MR-Sort with dominating vetoes

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "dV", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# MR-Sort with dominating dictators

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "Dv", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# MR-Sort with vetoes weakened by dictators

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "v", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# MR-Sort with dictators weakened by vetoes

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "d", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model does not fit

m3$humanReadableStatus

# MR-Sort with conflicting vetoes and dictators

m3 <- LPDMRSortInferenceExact(pT, assig, categoriesRanks, criteriaMinMax,
                              majorityRule = "dv", readableWeights = TRUE,
                              readableProfiles = TRUE, minmaxLPD = TRUE, 
                              solver = "cplex", alternativesIDs = names(assig))

# model fits

m3$humanReadableStatus

# we stop the protocol and extract the assignments rules

extractLPDMRSortRules(m3$lambda,m3$weights,criteriaMinMax,criteriaUBs,criteriaLBs,m3$profilesPerformances,categoriesRanks,m3$vetoPerformances,m3$dictatorPerformances,"dv")