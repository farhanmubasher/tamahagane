
tamahagane.apply.join<- function(x,y,joinType)
{
  x <- fromJSON(x)
  y <- fromJSON(y)
  joinType <- fromJSON(joinType)
  
  if(joinType=="" || joinType== "inner")
  {
    return(merge(x,y))
  }
  else if (joinType=="left")
  {
    return (merge(x,y, all.x=TRUE)) 
  }
  else if (joinType=="right")
  {
    return (merge(x,y, all.y= TRUE))
  }
  else if(joinType== "outer")
  {
   return( merge(x,y, all= TRUE))
  }
}
