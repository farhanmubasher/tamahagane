
tamahagane.apply.filter<- function(dataset, columName, filterType="", object)
  {
  converted.dataset<- fromJSON(dataset)
  attribute<- fromJSON(columName)

  obj <- paste("^", object, sep = "")

  converted.attribute<- converted.dataset[,attribute]


    if(filterType=="")
  {
      warning("Please specify your filter type or Object !!")
  }
  else{

    if(filterType=="EQUAL"){
      return( subset(converted.dataset, converted.attribute == c(object) ))
    }
    else if (filterType== "NOTEQUAL"){

     return( subset(converted.dataset, converted.attribute != c(object)  ))
    }
    else if(filterType== "ISIN"){

      return( subset(converted.dataset, converted.attribute %in% c(object)  ))

    }
    else if(filterType== "ISNOTIN")
    {
      return( subset(converted.dataset, converted.attribute != c(object)  ))

    }
    else if(filterType=="STARTWITH"){

      return( converted.dataset[grep(obj, converted.dataset[,attribute]) ,])
    }
     else if (filterType== "ENDWITH"){

      return( subset(converted.dataset, converted.attribute == c(object)  ))
    }
    else if (filterType== "MATCHES"){

      return( subset(converted.dataset, converted.attribute == c(object)  ))
    }
   else if (filterType== "NOTMATCHES"){

      return( subset(converted.dataset, converted.attribute != c(object)  ))
    }
  }

}
