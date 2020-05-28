ELECTREIIIDistillation <-
function(performanceTable,criteriaWeights,minMaxcriteria,preferenceThresholds,indifferenceThresholds,vetoThresholds){
  # data is filtered, check for some data consistency
  
  # checking minimum of 2 criteria 
  if (is.null(dim(performanceTable))) 
    stop("performanceTable must have 2 criterias and alternatives")
  
  ## check the input data
  if (!((is.matrix(performanceTable) || (is.data.frame(performanceTable)))))  #performanceTable must be a matrix
      stop("wrong performanceTable, must be a matrix or a data frame")
  
  if (!(is.vector(minMaxcriteria)))
      stop("minMaxcriteria must be defined as a vector")
  minMaxcriteria=tolower(minMaxcriteria)
  if(!(ncol(performanceTable)==length(minMaxcriteria)))
      stop("length of criteriaMinMax should be checked") 
  
  n=length(minMaxcriteria)	
  for (i in 1:n){
      if(!((minMaxcriteria[i]=='min') ||(minMaxcriteria[i]=='max'))){
          stop(" Vector minMaxcriteria must contain 'max' or 'min' ")
      }
  }
        
  if (!(is.vector(criteriaWeights)))
      stop("criteriaWeights should be a vector")
  if (!is.numeric(criteriaWeights))	
      stop("criteriaWeights should be a numeric vector") 
  if(!(ncol(performanceTable)==length(criteriaWeights)))
      stop("length of criteriaWeights should be checked") 
        
  ##  End of checking the validity of the inputs
        
  #  Variables transformation
  
  perf  <- performanceTable     # renaming of parameters
  alt  <-  rownames(perf)
  crit  <- colnames(perf)
  wgt <- criteriaWeights
  critMM  <- minMaxcriteria
  pref<-preferenceThresholds
  indif<-indifferenceThresholds
  veto<-vetoThresholds

  n=nrow(perf)
  if (!(is.na(match("min", critMM)))) {
      for(j in 1:ncol(perf)){
          valmax=max(perf[,j])
          if (critMM[j]=="min") {
              for (i in 1:nrow(perf)){
                  perf[i,j]=valmax-perf[i,j]
              }
          }
      }
      
  }
       
  #  Calculus of concordance matrix
  concordance <- matrix (rep(0, n*n), n, n) # creation of the concordance matrix                    # 
  for (i in 1:nrow(perf))
  { # looping in the rows of performanceTable with i variable       			
    for (j in 1:nrow(perf))
    { # looping in the rows of performanceTable with j variable
      c1=0 # using this variable for storage the sum of weigths
      for (k in 1:ncol(perf))
      { # looping in the columns of performanceTable with k variable
        if(critMM[k]=="max")
        { #case of maximum 
          if(perf[i,k] >= perf[j,k] - indif[k])
          { #condition : if the term i is greater than j
            c1=c1+wgt[k] #c1 takes the value of the associate weight
          }
          else if(perf[i,k] >= perf[j,k] - pref[k])
          {
            c1=c1+(perf[i,k]+pref[k]-perf[j,k])/(pref[k]-indif[k])*wgt[k]
          }
        }
			  if(critMM[k]=="min")
		    { #case of minimum
          if (perf[i,k] <= perf[j,k] + indif[k])
          { #condition : if the term i is greater than j
            c1=c1+wgt[k] #c1 takes the value of the associate weight
          }
			    else if(perf[i,k] <= perf[j,k] + pref[k])
			    {
			      c1=c1+(perf[j,k] + pref[k] - perf[i,k])/(pref[k]-indif[k])*wgt[k]
			    }
			  }
      }
      concordance[i,j]=c1/sum(wgt)   #the term of the matrix takes the value of c1 divided by the sum of weight
    }
  }
        
  #  Computation of credibility matrix
  credibility <- matrix (rep(0, n*n), n, n) # creation of the concordance matrix                    # 
  for (i in 1:nrow(perf))
  { # looping in the rows of performanceTable with i variable       			
    for (j in 1:nrow(perf))
    { # looping in the rows of performanceTable with j variable
      credibility[i,j] <- concordance[i,j]
      for (k in 1:ncol(perf))
      { # looping in the columns of performanceTable with k variable
        discordance = 0
        if(critMM[k]=="max")
        { #case of maximum 
          if(perf[i,k] <= perf[j,k] - veto[k])
          { #condition : if the term i is greater than j
            discordance=1
          }
          else if(perf[i,k] < perf[j,k] - pref[k])
          {
            discordance = (perf[i,k]+veto[k]-perf[j,k])/(veto[k]-pref[k])
          }
        }
        if(critMM[k]=="min")
        { #case of minimum
          if (perf[i,k] >= perf[j,k] + veto[k])
          { #condition : if the term i is greater than j
            discordance=1
          }
          else if(perf[i,k] <= perf[j,k] + pref[k])
          {
            discordance = (perf[j,k] + pref[k] - veto[i,k])/(veto[k]-pref[k])
          }
        }
        if(discordance >= concordance[i,j])
        {
          credibility[i,j] <- credibility[i,j] * (1-discordance)/(1-concordance[i,j])
        }
      }
    }
  }
  
  distilationdesc <- list()
  remaining <-1:n
  lambda <- unique(sort(credibility, decreasing = TRUE))
  l <- 1
  while(length(remaining) > 0 && l < length(lambda))
  {
    cut <- apply(credibility,1:2,function(x) if(x >= lambda[l]) 1 else 0)
    scores <- sapply(remaining,function(x) sum(cut[x,remaining]) - sum(cut[remaining,x]))
    maxscore <- max(scores)
    distilationdesc[[l]] <- alt[remaining[sapply(1:length(scores),function(x) scores[x] == maxscore)]]
    remaining <- remaining[!sapply(1:length(scores),function(x) scores[x] == maxscore)]
    l <- l+1
  }
  
  distilationasc <- list()
  remaining <-1:n
  lambda <- unique(sort(credibility, decreasing = TRUE))
  l <- 1
  while(length(remaining) > 0 && l < length(lambda))
  {
    cut <- apply(credibility,1:2,function(x) if(x >= lambda[l]) 1 else 0)
    scores <- sapply(remaining,function(x) sum(cut[x,remaining]) - sum(cut[remaining,x]))
    minscore <- min(scores)
    distilationasc[[l]] <- alt[remaining[sapply(1:length(scores),function(x) scores[x] == minscore)]]
    remaining <- remaining[!sapply(1:length(scores),function(x) scores[x] == minscore)]
    l <- l+1
  }
  distilationasc <- rev(distilationasc)
  
  list(distilationDesc = distilationdesc, distilationAsc = distilationasc)
}