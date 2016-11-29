tamahagane.apply.join<- function(x,y,joinType="")
{

  if(joinType=="" || joinType== "inner")
  {
    merge(x,y)
  }
  else if (joinType=="left")
  {
    merge(x,y, all.x=TRUE)

  }
  else if (joinType=="right")
  {
    merge(x,y, all.y= TRUE)
  }
  else if(joinType== "outer")
  {
    merge(x,y, all= TRUE)
  }


}
