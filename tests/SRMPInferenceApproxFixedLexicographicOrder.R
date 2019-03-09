# ranking some students

library(MCDA)

# fix seed

set.seed(1234)

# SRMP model

referenceProfiles <- replicate(3, c(0.2,0.5,0.8))

lexicographicOrder <- c(2,1,3)

weights <- c(0.2,0.44,0.36)

# the performance table

performanceTable <- replicate(3, runif(5)) 

criteriaMinMax <- c("max","max","max")

rownames(performanceTable) <- c("a1","a2","a3","a4","a5")

colnames(performanceTable) <- c("c1","c2","c3")

names(criteriaMinMax) <- colnames(performanceTable)

# expected result for the tests below

expectedValues <- SRMP(performanceTable, referenceProfiles, lexicographicOrder, weights, criteriaMinMax)

names(expectedValues) <- rownames(performanceTable)

# test - preferences and indifferences

preferencePairs <- c()
indifferencePairs <- c()

for(i in 1:4)
{
  for(j in (i+1):5)
  {
    if(expectedValues[[i]] > expectedValues[[j]])
    {
      preferencePairs <- rbind(preferencePairs, c(rownames(performanceTable)[i],rownames(performanceTable)[j]))
    }
    else if(expectedValues[[i]] < expectedValues[[j]])
    {
      preferencePairs <- rbind(preferencePairs, c(rownames(performanceTable)[j],rownames(performanceTable)[i]))
    }
    else
    {
      indifferencePairs <- rbind(indifferencePairs, c(rownames(performanceTable)[i],rownames(performanceTable)[j]))
    }
  }
}

result<-SRMPInferenceApproxFixedLexicographicOrder(performanceTable, criteriaMinMax, lexicographicOrder, preferencePairs, indifferencePairs)

alternativesValues<-SRMP(performanceTable, result$referenceProfiles, lexicographicOrder, result$criteriaWeights, criteriaMinMax)

stopifnot(all(alternativesValues == expectedValues))
