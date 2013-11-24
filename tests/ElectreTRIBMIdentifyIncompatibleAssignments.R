# ranking some cars (from the original article on UTA by Siskos and Lagreze, 1982)

library(MCDA)

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
  "Peugeot505GR",
  "OpelRecord2000LS",
  "CitroenVisaSuperE",
  "VWGolf1300GLS",
  "CitroenCX2400Pallas",
  "Mercedes230",
  "BMW520",
  "Volvo244DL",
  "Peugeot104ZS",
  "CitroenDyane")

colnames(performanceTable) <- c(
  "MaximalSpeed",
  "ConsumptionTown",
  "Consumption120kmh",
  "HP",
  "Space",
  "Price")

assignments <-c("Good","Bad","Good","Bad","Medium","Medium","Medium","Good","Bad","Good")

names(assignments) <- rownames(performanceTable)

categoriesRanks <-c(1,2,3)

names(categoriesRanks) <- c("Good","Medium","Bad")

criteriaMinMax <- c("max","min","min","max","max","min")

names(criteriaMinMax) <- colnames(performanceTable)

onoff<-ElectreTRIBMIdentifyIncompatibleAssignments(performanceTable, assignments, categoriesRanks, criteriaMinMax, criteriaIDs = c("HP","Space","Price","MaximalSpeed","ConsumptionTown"))

filteredAlternativesIDs <- rownames(performanceTable)[!onoff$incompatibleAssignments]

x<-ElectreTRIBMInference(performanceTable, assignments, categoriesRanks, criteriaMinMax,  criteriaIDs = c("HP","Space","Price","MaximalSpeed","ConsumptionTown"), alternativesIDs = filteredAlternativesIDs)
 
ElectreAssignments<-ElectreTRIBM(performanceTable, x$profilesPerformances, 
                         x$weights, criteriaMinMax, x$lambda, criteriaIDs = c("HP","Space","Price","MaximalSpeed","ConsumptionTown"), alternativesIDs = filteredAlternativesIDs)

print(all(ElectreAssignments == assignments[!onoff$incompatibleAssignments]))

stopifnot(all(assignments[!onoff$incompatibleAssignments] == ElectreAssignments))
