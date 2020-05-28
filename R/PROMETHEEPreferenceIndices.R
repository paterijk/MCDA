PROMETHEEPreferenceIndices<- function(performanceTable, preferenceFunction, preferenceThreshold, indifferenceThreshold, gaussParameter, criteriaWeights, criteriaMinMax)
  
{ 
  # check the input data
  
  numAlt<-dim(performanceTable)[1] # number of alternatives
  numCrit<-dim(performanceTable)[2] # number of criteria
  
  if (!(is.matrix(performanceTable)))
    stop("wrong performanceTable, should be a matrix")
  
  if (!(is.vector(preferenceFunction)))
    stop("preferenceFunction should be a vector")
  
  for (j in (1:numCrit))
  {
    if (!(preferenceFunction[j] %in% c("Usual","U-shape","V-shape","Level","V-shape-Indiff","Gaussian")))
    {
      stop("wrong preferenceFunction, should be equal to Usual,U-shape,V-shape,Level,V-shape-Indiff or Gaussian")
    }
  }
   
  
  if (!(is.vector(preferenceThreshold)))
    stop("preferenceThreshold should be a vector")
  
  if (!(is.vector(indifferenceThreshold)))
    stop("indifferenceThreshold should be a vector")
  
  if (!(is.vector(gaussParameter)))
    stop("gaussParameter should be a vector")
  
  
  if (!(is.vector(criteriaMinMax)))
    stop("criteriaMinMax should be a vector")
  
  if (!(is.vector(criteriaWeights)))
    stop("criteriaWeights should be a vector")
  # -------------------------------------------------------
  

  preferenceTable<-matrix(rep(0,numAlt*numAlt),numAlt,numAlt)
#Pairwise comparisons of evaluation criteria
  for(i in (1:numAlt)){
    for(j in (1:numAlt)){
      if (i==j)
        preferenceTable[i,j]=0
      else
      {
      for(l in (1:numCrit)){
        d<-performanceTable[i,l]-performanceTable[j,l]
          d1<- -d
#Definition of the six types of preference functions
          if (preferenceFunction[l]=='Usual' & criteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1
#Definition of matrix (numAlt x numAlt) containing the aggregated preference indices 
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Usual' & criteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='U-shape' & criteriaMinMax [l]=='max'){
            if (d>indifferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='U-shape' & criteriaMinMax[l]=='min'){
            if (d1>indifferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
          }
          
          else if (preferenceFunction[l]=='V-shape' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>=0)){
              Pl=d/(preferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='V-shape' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>=0)){
              Pl=d1/(preferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Level' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>indifferenceThreshold[l])){
              Pl=0.5
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='Level' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>indifferenceThreshold[l])){
              Pl=0.5
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='V-shape-Indiff' & criteriaMinMax[l]=='max'){
            if (d>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
            else if ((d<=preferenceThreshold[l])&(d>indifferenceThreshold[l])){
              Pl=(d-indifferenceThreshold[l])/(preferenceThreshold[l]-indifferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='V-shape-Indiff' & criteriaMinMax[l]=='min'){
            if (d1>preferenceThreshold[l]){
              Pl=1
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            else if ((d1<=preferenceThreshold[l])&(d1>indifferenceThreshold[l])){
              Pl=(d1-indifferenceThreshold[l])/(preferenceThreshold[l]-indifferenceThreshold[l])
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          
          else if (preferenceFunction[l]=='Gaussian' & criteriaMinMax[l]=='max'){
            if (d>0){
              Pl=1-exp(-((d^2)/(2*gaussParameter[l]^2)))
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
          }
          else if (preferenceFunction[l]=='Gaussian' & criteriaMinMax[l]=='min'){
            if (d1>0){
              Pl=1-exp(-(((d1)^2)/(2*gaussParameter[l]^2)))
              preferenceTable[i,j]<-preferenceTable[i,j]+Pl*criteriaWeights[l]}
            
          }
    }
    }
    }
  }
  rownames(preferenceTable) <- rownames(performanceTable)
  colnames(preferenceTable) <- rownames(performanceTable)
  return(preferenceTable)
}