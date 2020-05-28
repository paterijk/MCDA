\name{PROMETHEEPreferenceIndices}
\alias{PROMETHEEPreferenceIndices}

\title{Preference indices for the PROMETHEE methods}

\description{This function computes the preference indices from a performance table based on the given function types and parameters for each criterion.}

\usage{
PROMETHEEPreferenceIndices(performanceTable, preferenceFunction,
                           preferenceThreshold,indifferenceThreshold,
                           gaussParameter,criteriaWeights,criteriaMinMax)
}

\arguments{
  \item{performanceTable}{Matrix containing the performance table. 
    Each row corresponds to an alternative, and each column to a criterion. 
    Rows (resp. columns) must be named according to the IDs of the alternatives (resp. criteria).}
    
  \item{preferenceFunction}{A vector containing the names of the preference functions to be used. preferenceFunction should be equal to Usual, U-shape, V-shape,
  Level, V-shape-Indiff or Gaussian. The elements of the vector are named according to the IDs of the criteria.}
  \item{preferenceThreshold}{A vector containing thresholds of strict preference. The elements are named according to the IDs of the criteria.}
  \item{indifferenceThreshold}{A vector containing thresholds of indifference. The elements are named according to the IDs of the criteria.}
  \item{gaussParameter}{A vector containing parameters of the Gaussian preference function. The elements are named according to the IDs of the criteria.}
    \item{criteriaWeights}{Vector containing the weights of the criteria. 
    The elements are named according to the IDs of the criteria.}
  \item{criteriaMinMax}{Vector containing the preference direction on each of the criteria. 
    "min" (resp. "max") indicates that the criterion has to be minimized (maximized). 
    The elements are named according to the IDs of the criteria.}
}

\value{
  The function returns a matrix containing all the aggregated preference indices.
}

\examples{
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


#Preference indices

preferenceTable<-PROMETHEEPreferenceIndices(performanceTable, preferenceFunction,
                                            preferenceThreshold, indifferenceThreshold,
                                            gaussParameter, criteriaWeights,
                                            criteriaMinMax)

}