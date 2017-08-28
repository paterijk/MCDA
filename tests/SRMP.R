# ranking some students

library(MCDA)

# the performance table

performanceTable <- rbind(c(10,10,9),c(10,9,10),c(9,10,10),c(9,9,10),c(9,10,9),c(10,9,9),
                          c(10,10,7),c(10,7,10),c(7,10,10),c(9,9,17),c(9,17,9),c(17,9,9),
                          c(7,10,17),c(10,17,7),c(17,7,10),c(7,17,10),c(17,10,7),c(10,7,17),
                          c(7,9,17),c(9,17,7),c(17,7,9),c(7,17,9),c(17,9,7),c(9,7,17))

referenceProfiles <- rbind(c(5,5,5),c(10,10,10),c(15,15,15))

lexicographicOrder <- c(2,1,3)

weights <- c(0.2,0.44,0.36)

criteriaMinMax <- c("max","max","max")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24")

colnames(performanceTable) <- c("c1","c2","c3")

colnames(referenceProfiles) <- c("c1","c2","c3")

names(weights) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

expectedValues <- c(10,7,13,3,5,1,10,7,13,4,6,2,14,12,8,15,11,9,4,6,2,6,2,4)

names(expectedValues) <- rownames(performanceTable)

alternativesValues <- SRMP(performanceTable, referenceProfiles, lexicographicOrder, weights, criteriaMinMax)

print(alternativesValues)

stopifnot(all(alternativesValues == expectedValues))