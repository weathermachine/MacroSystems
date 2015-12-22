machange <- function(df,column,frontend,backend,arithorgeom) 
{
  newcol <- paste(column,"_",frontend,"x",backend,"mma",sep="")
  
  if (arithorgeom == "Arith") {
      mutatecall <- interp(~(rollapply(x,frontend,mean,fill=NA,align="right")-
                           rollapply(x,backend,mean,fill=NA,align="right")), 
                           x = as.name(column))
      df %>% mutate_(.dots = setNames(list(mutatecall),newcol))
  }
  



}


machange <- function(df,column,frontend,backend,arithorgeom) 
{
  
  newcol <- paste(column,"_",frontend,"x",backend,"mma",sep="")
  
  if (arithorgeom == "Arith") {
    mutatecall <- interp(~(rollapply(x,frontend,mean,fill=NA,align="right")-
                           rollapply(x,backend,mean,fill=NA,align="right")), 
                           x = as.name(column))
    df %>% mutate_(.dots = setNames(list(mutatecall),newcol))
  }

  else if (arithorgeom == "Geom") {
    mutatecall <- interp(~ exp(
      rollapply(log(x),frontend,mean,fill=NA,align="right")-
        rollapply(log(x),backend,mean,fill=NA,align="right"))-1, 
      x = as.name(column)) 
    
    df %>% mutate_(.dots = setNames(list(mutatecall),newcol))
  }
  
  
}



rollz <- function(df,column,window) 
{
  newcol <- paste(column,"_",window,"mZ",sep="")
  mutatecall <- interp(~(x - 
                       rollapply(x,window,mean,fill=NA,align="right")) /
                       rollapply(x,window,  sd,fill=NA,align="right"), 
                   x = as.name(column))
  df %>% mutate_(.dots = setNames(list(mutatecall),newcol))
}


#sample SE function
f = function(df,col1, col2, new_col_name) {
  mutate_call = lazyeval::interp(~ a + b, a = as.name(col1), b = as.name(col2))
  df %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))
}

