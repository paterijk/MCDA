#############################################################################
#
# Copyright Patrick Meyer, Sébastien Bigaret, Richard Hodgett, Alexandru-Liviu Olteanu and Sajid Siraj, 2019
#
# Contributors:
#   Patrick Meyer <patrick.meyer@telecom-bretagne.eu>
#   Sebastien Bigaret <sebastien.bigaret@telecom-bretagne.eu>
#   Richard Hodgett <r.e.hodgett@leeds.ac.uk>
#   Alexandru-Liviu Olteanu
#   Sajid Siraj <sajidsiraj@gmail.com>
#		
# This software, MCDA, is a package for the R statistical software which 
# allows to use MCDA algorithms and methods. 
# 
# This software is governed by the CeCILL license (v2) under French law
# and abiding by the rules of distribution of free software. You can
# use, modify and/ or redistribute the software under the terms of the
# CeCILL license as circulated by CEA, CNRS and INRIA at the following
# URL "http://www.cecill.info".
# 
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#		
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#		
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
##############################################################################

Concordance <- function(scores, q, p, w ){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  ## if weights are not provided, then assign equal weight to all criteria
  if(is.null(w) || !is.vector(w))
    w = rep(1.0/numCriteria, numCriteria)
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(q))
    stop("The indifference thresholds q dimensions mismatch with the table of scores")
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(w))
    stop("The weights vector w dimensions mismatch with the table of scores")
  
  ## partial concordance Cx = p-(b-a) / (p-q)
  Cx <- array(NA, c(numOptions,numOptions,numCriteria))
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Cx[i,j,k] <- (p[k] - (scores[j,k]-scores[i,k]) )/(p[k]-q[k])
      }
    }
  }
  ## clip the values above 1 and below 0
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Cx[i,j,k] <- ifelse(Cx[i,j,k]>1, 1, ifelse(Cx[i,j,k]<0, 0,Cx[i,j,k]) ) 
      }
    }
  }
  ## overall concordance
  concordance <- array(NA, c(numOptions,numOptions))
  for(i in 1:numOptions){
    for(j in 1:numOptions){
      concordance[i,j] <- 0
      for(k in 1:numCriteria){
        concordance[i,j] = concordance[i,j] + (w[k]*Cx[i,j,k])
      }
    }
  }
  
  return(concordance)
  
}

Discordance <- function(scores, p, v){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(v))
    stop("The veto thresholds v dimensions mismatch with the table of scores")
  
  
  ## partial discordance Dx = (b-a)-p / (v-p)
  Dx <- array(NA, c(numOptions,numOptions,numCriteria))
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Dx[i,j,k] = ((scores[j,k]-scores[i,k]) - p[k] ) / (v[k] - p[k])
      }
    }
  }
  ## clip the values above 1 and below 0
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Dx[i,j,k] <- ifelse(Dx[i,j,k]>1, 1, ifelse(Dx[i,j,k]<0, 0,Dx[i,j,k]) ) 
      }
    }
  }
  ## overall discordance (we do not aggregate discordances)
  discordance <- Dx
  return(discordance)
  
}

Credibility <- function(scores, concordance, discordance ){
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  
  ## credibility to combine concordance and discordances
  credibility <- array(NA, c(numOptions,numOptions))
  for(j in 1:numOptions){
    for(i in 1:numOptions){
      credibility[i,j] <- concordance[i,j]
      for(k in 1:numCriteria){
        if(discordance[i,j,k]>concordance[i,j]){
          credibility[i,j] <- credibility[i,j]*(1-discordance[i,j,k])/(1-concordance[i,j]) 
        }
      }
    }
  }
  
  return(credibility)
  
}


Distillation <- function(credibility, threshold=0.3){
  
  N <- nrow(credibility)
  highest <- max( as.vector(credibility) )
  cutoff <- highest - threshold
  
  dominance <- array(NA, c(N,N))
  for(i in 1:N){
    for(j in 1:N){
      dominance[i,j] <- ifelse(credibility[i,j]>cutoff, 1, 0)
    }
  }
  
  RowSum <- rep(0,N)
  ColSum <- rep(0,N)
  for(i in 1:N){
    for(j in 1:N){
      RowSum[i] <- RowSum[i] + dominance[i,j]
      ColSum[i] <- ColSum[i] + dominance[j,i]
    }
  }
  scoring <- rep(NA,N)
  for(i in 1:N){
    scoring[i] <- RowSum[i] - ColSum[i]
  }
  
  outputs <- list(dominance = dominance, scoring = scoring )
  return(outputs)
  
}

ELECTRE3 <- function(scores, q, p, v, w ){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  ## if weights are not provided, then assign equal weight to all criteria
  if(is.null(w) || !is.vector(w))
    w = rep(1.0/numCriteria, numCriteria)
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(q))
    stop("The indifference thresholds q dimensions mismatch with the table of scores")
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(v))
    stop("The veto thresholds v dimensions mismatch with the table of scores")
  if(numCriteria != length(w))
    stop("The weights vector w dimensions mismatch with the table of scores")
  
  concordance <- Concordance(scores, p=p, q=q, w=w)
  discordance <- Discordance(scores, p=p, v=v)  
  credibility <- Credibility(scores, concordance=concordance, discordance=discordance)
  res <- Distillation(credibility)
  
  outputs <- list(concordance = concordance, 
                  discordance = discordance, 
                  credibility = credibility,
                  dominance = res$dominance,
                  scoring = res$scoring)
  return(outputs)
  
}