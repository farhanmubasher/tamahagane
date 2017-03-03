library(plyr)
tamahagane.apply.join<- function(x,y,joinType)
{
  x <- fromJSON(x)
  y <- fromJSON(y)
  joinType <- fromJSON(joinType)
  result <- join(x, y, type= joinType)
  result[is.na(result)] <- "?"
  if(nrow(result) == 0){
      return("No Records Found !!")
  }
  else{
    return(result)
  }
}
