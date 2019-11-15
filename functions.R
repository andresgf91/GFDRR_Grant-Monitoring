
# PACKAGES ----------------------------------------------------------------

library(dplyr)



# WRANGLING ---------------------------------------------------------------

vlookup <- function (df,x,col_ref,col_des){ 
  ls <- c()
  count <- 1
  for (i in x){
    if(i %in% df[col_ref][,1]){
      pos <-which(df[col_ref]==i)
      ls[count] <- df[col_des][pos,]}
    else{ls[count] <- NA}
    count <- count+1
  }
  return(ls)
}


