tamahagane.sort <- function(Dataset, columName, sortType)
{
  Dataset <- fromJSON(Dataset)
  columName <- fromJSON(columName)
  sortType <- fromJSON(sortType)
  if(sortType=="ASC" || sortType=="")
  {
    return (Dataset[order(Dataset[, columName]) ,])
  }
  else(sortType=="DESC")
  {
    return (Dataset[rev(order(Dataset[, columName]))  ,])
    
  }
  
}
