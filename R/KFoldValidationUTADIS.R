library(MCDA)





KFoldCrossValidationUTADIS <- function(performanceTable, criteriaMinMax, criteriaNumberOfBreakPoints, alternativesAssignments, categoriesRanks, epsilon, criteriaLBs=NULL, criteriaUBs=NULL, alternativesIDs = NULL, criteriaIDs = NULL, categoriesIDs = NULL, nbTest,k){

badAlt<-c()
goodAlt<-c()

for (i in 1:length(criteriaMinMax)){
	badAlt<-c(badAlt,min(performanceTable[,i]))
	goodAlt<-c(goodAlt,max(performanceTable[,i]))
	}

categoriesIDs=names(categoriesRanks)

errorTable<-rep(0,length(categoriesRanks))
names(errorTable)<-0:(length(categoriesRanks)-1)
errorMatrix<-matrix(0,nrow=length(categoriesRanks),ncol=length(categoriesRanks))
rownames(errorMatrix)<-names(categoriesRanks)
colnames(errorMatrix)<-names(categoriesRanks)
nbAlt<-nrow(performanceTable)
count<-0
if(is.null(alternativesIDs)){
	alternativesIDs=names(alternativesAssignments)
	}
worstCategory=names(categoriesRanks)[length(categoriesRanks)]
bestCategory=names(categoriesRanks)[1]

print("Preference information")
print(cbind(performanceTable,alternativesAssignments))
print(cat("Starting the ",k,"-fold validation process of MR-Sort with ",nbTest," tests                                                                                                      "))


for (i in 1:nbTest){# We will make "nbTest" tests. In each of these tests we will part the learning set in k parts that we call folds.

	print(cat("                   -----    -----    -----      Starting test n ",i,"    -----    -----    -----                                                                 "))
	print("----------------------------------------------------------------------------------------------------------")
	print("----------------------------------------------------------------------------------------------------------")
	L<-sample(nbAlt)
	localCount<-0
	for (j in 1:k){ # Then we will successively take one fold off and learn with the k-1 other folds. The model thus obtained is called x.
		Lout<-L[((j-1)*nbAlt/k+1):(j*nbAlt/k)]
		Lin<-setdiff(L,Lout)
		Lout<-sort(Lout)
		Lin<-sort(Lin)
		altIn=alternativesIDs[Lin]
		altOut=alternativesIDs[Lout]
		alternativesAssignmentsIn=alternativesAssignments[Lin]
		alternativesAssignmentsOut=alternativesAssignments[Lout]
		performanceTableIn=performanceTable[Lin,]
		performanceTableOut=performanceTable[Lout,]

		print(cat("         Extract : ",Lout,"                                                                                                                                  "))

		x<-UTADIS(criteriaLBs=badAlt,criteriaUBs=goodAlt,performanceTable=performanceTableIn,
				criteriaMinMax=criteriaMinMax,criteriaNumberOfBreakPoints=criteriaNumberOfBreakPoints,alternativesAssignments=alternativesAssignmentsIn,categoriesRanks=categoriesRanks,epsilon=epsilon)
		found<-ClassifyWithUTADIS(x=x,performanceTableToClassify=performanceTable,lowestCategory=worstCategory)

		if(i==1){
		print(x)
		}


		# We look at how the preferences that were included in the taken fold are classified with x.
		
		print("Found")
		print(found[Lout])
		print("Given")
		print(alternativesAssignments[Lout])	
		print(sum(found[Lout]!=alternativesAssignments[Lout]))
		localCount<-localCount+sum(found[Lout]!=alternativesAssignments[Lout])
		for(k1 in Lout){
			errorMatrix[alternativesAssignments[k1],found[k1]]<-errorMatrix[alternativesAssignments[k1],found[k1]]+1
			errorTable[abs(categoriesRanks[found[k1]]-categoriesRanks[alternativesAssignments[k1]])+1]<-errorTable[abs(categoriesRanks[found[k1]]-categoriesRanks[alternativesAssignments[k1]])+1]+1
		}
		print("                  ----------------------                 -------------------- ")
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
print("Final result")
print("    Error Table")
print(errorTable/nbTot)
print("    Error Matrix  (row <- given and col <- found)")
print(errorMatrix/nbTest)
print("    Misclassification rate")
print(count/nbTot)



return(count/nbTot)

}



ClassifyWithUTADIS <- function(x,performanceTableToClassify,lowestCategory,alternativesIDs=NULL){

tableUti<-applyPiecewiseLinearValueFunctionsOnPerformanceTable(x$valueFunctions,performanceTableToClassify,alternativesIDs=alternativesIDs)
scores<-weightedSum(tableUti,c(1,1,1))
categoryBis=c(x$categoriesLBs,0)
nameVector<-c(names(x$categoriesLBs),lowestCategory)
names(categoryBis)<-nameVector
AssignCat<-assignAlternativesToCategoriesByThresholds(scores,categoriesIDs=nameVector,categoriesLowerBounds=categoryBis,alternativesIDs=alternativesIDs)
return(AssignCat)
}








