pairwise_href <- function(spdf1,spdf2, method="mean"){
  href_list <- unlist(lapply(list(spdf1@coords,spdf2@coords), function(x){
    (dim(x)[1]^(-1/6) * (0.5* (sd(x[,1]) + sd(x[,2]))))
  }))
  
  if(method=="mean"){
    return(mean(href_list))
  } else if(method=="max"){
    return(max(href_list))
  } else if(method=="min"){
    return(min(href_list))
  } else{error("ERROR: Unsupported method to calculate h for kernelUD.")}
  
}