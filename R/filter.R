
tamahagane.apply.filter<- function(dataset, attribute, filterType="", object="")
  {

    if(filterType=="" || object=="")
  {
      warning("Please specify your filter type or Object !!")
  }
  else{

    if(filterType=="EQUAL"){
      return( subset(dataset, attribute == object ))
    }
    else if (filterType== "NOTEQUAL"){

     return( subset(dataset, attribute != object ))
    }
    else if(filterType== "ISIN"){

      return( subset(dataset, attribute %in% object ))

    }
    else if(filterType== "ISNOTIN")
    {
      return( subset(dataset, attribute != object ))

    }
    else if(filterType=="STARTWITH"){
      return( subset(dataset, attribute == object ))
    }
     else if (filterType== "ENDWITH"){

      return( subset(dataset, attribute == object ))
    }
    else if (filterType== "MATCHES"){

      return( subset(dataset, attribute == object ))
    }
   else if (filterType== "NOTMATCHES"){

      return( subset(dataset, attribute != object ))
    }
  }

}


