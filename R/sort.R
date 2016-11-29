tamahagane.sort <- function(attribute, sortType="")
{

  if(sortType=="ASC" || sortType=="")
  {
    return (sort(attribute))
  }
  else(sortType=="DESC")
  {
    return (rev(sort(attribute)))

  }

}
