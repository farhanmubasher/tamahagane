
tamahagane.apply.filter<- function(dataset, attribute, filterType="", object)
  {
  converted.dataset<- fromJSON(dataset)

  converted.attribute<- converted.dataset[,attribute]


    if(filterType=="")
  {
      warning("Please specify your filter type or Object !!")
  }
  else{

    if(filterType=="EQUAL"){
      return( subset(converted.dataset, converted.attribute == object ))
    }
    else if (filterType== "NOTEQUAL"){

     return( subset(converted.dataset, converted.attribute != object ))
    }
    else if(filterType== "ISIN"){

      return( subset(converted.dataset, converted.attribute %in% object ))

    }
    else if(filterType== "ISNOTIN")
    {
      return( subset(converted.dataset, converted.attribute != object ))

    }
    else if(filterType=="STARTWITH"){
      return( subset(converted.dataset, converted.attribute == object ))
    }
     else if (filterType== "ENDWITH"){

      return( subset(converted.dataset, converted.attribute == object ))
    }
    else if (filterType== "MATCHES"){

      return( subset(converted.dataset, converted.attribute == object ))
    }
   else if (filterType== "NOTMATCHES"){

      return( subset(converted.dataset, converted.attribute != object ))
    }
  }

}


