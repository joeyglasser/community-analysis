



total_coverage_all_clusters  <-function(clusters, A){
  
  n = ncol(A)
  
  
  A00 = matrix(0,nc=n,nr=n)
  
  for( k in 1:length(clusters)  ){
    
    J = clusters[[k]]
    
    B_J =  A[J,J] 
    
    if( !is.matrix(B_J )  ){ 
      A00[J,J ]  <-   B_J  
    }else{
      
      for(i in seq_along(J)){
        for(j in seq_along(J)){
          if(A00[J,J][i,j]==0)  A00[J,J][i,j] <-   B_J[i,j]
        }
      }
      
    }
    
  }
  
  A_all = A00
  
  SL_accountedfor = sum(diag(A_all)) / sum(diag(A))
  total_crossplanted = sum( A_all  [ upper.tri(  A_all ,diag = F)]  )
  total_crossA= sum(A  [ upper.tri(A,diag = F)]  )
  
  nonSL_accountfor = total_crossplanted / total_crossA
  
  all_accountedfor = ( sum(diag(A_all)) + total_crossplanted ) / (   total_crossA +  sum(diag(A)) )
  
  
  list(
  nonSL_accountfor,
  SL_accountedfor,
  all_accountedfor
  )
}


find_total_accounted_for<-function(clusters, A){
  
  
  n = ncol(A)
  A00 = matrix(0,nc=n,nr=n)

  for( k in 1:length(clusters)  ){
    
    J = clusters[[k]]
    
    B_J =  A[J,J] 
    
    for(i in seq_along(J)){
      for(j in seq_along(J)){
        
        if(A00[J,J][i,j]==0)  A00[J,J][i,j] <-   B_J[i,j]
        
      }
    }
    
  }
  
  A_all = A00
  
  SL_accountedfor = sum(diag(A_all)) / sum(diag(A))
  total_crossplanted = sum( A_all  [ upper.tri(  A_all ,diag = F)]  )
  total_crossA= sum(A  [ upper.tri(A,diag = F)]  )
  
  nonSL_accountfor = total_crossplanted / total_crossA
  
  all_accountedfor = ( sum(diag(A_all)) + total_crossplanted ) / (   total_crossA +  sum(diag(A)) )
  
  return( list(SL_accountedfor, nonSL_accountfor, all_accountedfor))
}



implement_total_accounted<-function(msa2010,A ){
  
  make.Adj.mat<-function(E, n=n){
    
    A <-matrix(nc=n, nr = n, data = 0)
    
    for(k in 1:nrow(E)){
      a =  as.numeric(E[k,])
      A[a[1], a[2]] = a[3]
      A[a[2], a[1]] = a[3]
    }
    return(A)
    
  }

  
  
  library(plyr)
  
  
  inner_join(key , data.frame( fips = as.numeric(test) ))
  
  sort(sapply(msa2010, length ))
  
  
  msa_node = lapply(msa2010, function(x){
    inner_join(key , data.frame( fips = as.numeric(x) ))$node
  }   ) 
  
  A = make.Adj.mat(E,n = nrow(key))
  
  
  msa_coverage = total_coverage_all_clusters(msa_node, A)
  
  tot_clus = c(comms_cvt, hubspoke_cvt, island_cvt)
  
  unlabeled_clus = lapply(tot_clus, function(x){
    inner_join(key , data.frame( fips = as.numeric(x) ))$node
  }   ) 
  
  clus_coverage = total_coverage_all_clusters(unlabeled_clus, A)
  
  
  }


