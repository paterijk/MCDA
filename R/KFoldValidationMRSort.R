library(MCDA)

KFoldCrossValidationMRSort<- function(performanceTable, criteriaMinMax, alternativesAssignments, categoriesRanks,
						 alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL, nbTest,k){


if(is.null(alternativesIDs)){
	alternativesIDs<-names(alternativesAssignments)
	}
categoriesIDs=names(categoriesRanks)



#prepare the missclassification matrix and table
errorTable<-rep(0,length(categoriesRanks))
names(errorTable)<-0:(length(categoriesRanks)-1)
errorMatrix<-matrix(0,nrow=length(categoriesRanks),ncol=length(categoriesRanks))
rownames(errorMatrix)<-names(categoriesRanks)
colnames(errorMatrix)<-names(categoriesRanks)


#print the table of the preference information
print("Preference information")
print(cbind(performanceTable,alternativesAssignments))
print(cat("Starting the ",k,"-fold validation process of MR-Sort with ",nbTest," tests                                                                                                      "))
nbAlt<-nrow(performanceTable)
count<-0 # Counting the number of misclassification

for (i in 1:nbTest){# We will make "nbTest" tests. In each of these tests we will part the learning set in k parts that we call folds.
	print(cat("                   -----    -----    -----      Starting test n ",i,"    -----    -----    -----                                                                 "))
	print("----------------------------------------------------------------------------------------------------------")
	print("----------------------------------------------------------------------------------------------------------")
	L<-sample(nbAlt)
	localCount<-0
	for (j in 1:k){# Then we will successively take one fold off and learn with the k-1 other folds. The model thus obtained is called x.
		print("             ------------         ------------         ------------         ------------")		
		Lout<-L[((j-1)*nbAlt/k+1):(j*nbAlt/k)]
		Lin<-setdiff(L,Lout)#Indices of the alternatives that will be included in the learning set
		Lout<-sort(Lout)    #These alternatives will not be considered in the learning. We will try to classify them as close as possible to their original assignment
		print(cat("      Tested alternatives ",Lout,"                                                                                                            "))         
		Lin<-sort(Lin)
		altIn=alternativesIDs[Lin]
		altOut=alternativesIDs[Lout]
		x<-MRSortInferenceApprox(performanceTable=performanceTable,criteriaMinMax=criteriaMinMax,
		assignments=alternativesAssignments,categoriesRanks=categoriesRanks,alternativesIDs = altIn)
		if(i==1){
			print("MR-Sort model (first test only)")
			print(x)
		}
		found<-ClassifyWithMRSort(x=x,performanceTableToClassify=performanceTable,criteriaMinMax=criteriaMinMax,alternativesIDs = alternativesIDs)
		
		# We look at how the preferences that were included in the Lout fold are classified with x.
		
		print("          -          -            - ")
		print("Given")
		print(alternativesAssignments[altOut])
		print("Found")
		print(found[altOut])
		localCount<-localCount+sum(found[altOut]!=alternativesAssignments[altOut]) # And we count how many differences there are with the original classification
		print(sum(found[altOut]!=alternativesAssignments[altOut]))
		for(k1 in Lout){
			errorMatrix[alternativesAssignments[k1],found[k1]]<-errorMatrix[alternativesAssignments[k1],found[k1]]+1
			errorTable[abs(categoriesRanks[found[k1]]-categoriesRanks[alternativesAssignments[k1]])+1]<-errorTable[abs(categoriesRanks[found[k1]]-categoriesRanks[alternativesAssignments[k1]])+1]+1
		}

	}
	print("        --- Nb of diferences --- ")
	print(localCount)
	count<-count+localCount
}

nbTot<-(nbTest*(nbAlt-(nbAlt%%k)))


print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
print("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -")
print("")
print("")
print("Final result")
#print("    Error Table")
#print(errorTable/nbTot,3)
#print("    Error Matrix  (row <- given and col <- found)")
#print(errorMatrix/nbTest,3)
print("    Misclassification rate")
print(count/nbTot)



return(count/nbTot)

}





ClassifyWithMRSort<- function(x,performanceTableToClassify,criteriaMinMax,alternativesIDs=NULL){
return(MRSort(performanceTable=performanceTableToClassify, categoriesLowerProfiles=x$profilesPerformances, 
                                criteriaWeights=x$weights, criteriaMinMax=criteriaMinMax, majorityThreshold=x$lambda,
					  alternativesIDs=alternativesIDs))
}
