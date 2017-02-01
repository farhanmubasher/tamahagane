
tamahagane.apply.filter<- function(dataset, columName, filterType="", object)
  {
  converted.dataset<- fromJSON(dataset)
  attribute<- fromJSON(columName)

  obj.start <- paste("^", object, sep = "")
  obj.end <- paste(object, "$", sep = "")

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

     return( subset(converted.dataset, converted.attribute != object  ))
    }
    else if(filterType== "ISIN"){

      return( subset(converted.dataset, converted.attribute %in% object))

    }
    else if(filterType== "ISNOTIN")
    {
      return( subset(converted.dataset, converted.attribute != object ))

    }
    else if(filterType=="STARTWITH"){

      return( converted.dataset[grep(obj.start, converted.dataset[,attribute]) ,])
    }
     else if (filterType== "ENDWITH"){

       return( converted.dataset[grep(obj.end, converted.dataset[,attribute]) ,])
    }
    else if (filterType== "LESSTHAN"){

      return( subset(converted.dataset, converted.attribute < object ))
    }
   else if (filterType== "GREATERTHAN"){

      return( subset(converted.dataset, converted.attribute > object ))
    }
  }

}
