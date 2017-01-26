

tamahagne.read.csv <- function(file.path= ""){

  return (toJSON( read.csv(file.path, sep = ",", header = T)))
}
