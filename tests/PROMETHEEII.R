library(MCDA)

# The evaluation table

performanceTable <- rbind(
  c(1,10,1),
  c(4,20,2),
  c(2,20,0),
  c(6,40,0),
  c(30,30,3))
rownames(performanceTable) <- c("RER","METRO1","METRO2","BUS","TAXI")
colnames(performanceTable) <- c("Price","Time","Comfort")

# The preference functions 
preferenceFunction<-c("Gaussian","Level","V-shape-Indiff")

#Preference threshold
preferenceThreshold<-c(5,15,3)
names(preferenceThreshold)<-colnames(performanceTable)

#Indifference threshold
indifferenceThreshold<-c(3,11,1)
names(indifferenceThreshold)<-colnames(performanceTable)

#Parameter of the Gaussian preference function
gaussParameter<-c(4,0,0)
names(gaussParameter)<-colnames(performanceTable)

#weights

criteriaWeights<-c(0.2,0.3,0.5)
names(criteriaWeights)<-colnames(performanceTable)

# criteria to minimize or maximize

criteriaMinMax<-c("min","min","max")
names(criteriaMinMax)<-colnames(performanceTable)

PROMETHEEII(performanceTable, preferenceFunction,preferenceThreshold,indifferenceThreshold,gaussParameter,criteriaWeights,criteriaMinMax)
