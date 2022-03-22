
library(httr)
library(jsonlite)


id <- "8ad6e47b272643d8b6900dfca4443a80"
pw <- "619e2520b2a24a0a9563075591477dca"




getposcode <- function(x){
  
  
  a <- GET(paste0("https://api.addy.co.nz/search?key=8ad6e47b272643d8b6900dfca4443a80&","s=",x),authenticate(id,pw,type="basic"))
  b <- httr::content(a,type='text')
  c <- fromJSON(b, flatten = TRUE)
  output <- as.data.frame(cc$addresses)
  
  

  
  return(output)
 
  
}

test <- getposcode("Alexandra+112+russell")


# 
# getposcode("Alexandra+112+russell")
# id                                  a
# 1 2259910 112 Russell Street, Alexandra 9320
# id                                  a
# 1 2259910 112 Russell Street, Alexandra 9320
# 
# > 
#   
  
##error handling##
error_handle <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}


##Test##

test_retrieve <- error_handle(getposcode("Alexandra+112+russell"))

print(test_retrieve)

##get postcode of initial zone returned

retrieved <- as.String(test_retrieve$a.1)

retrieved[nchar(retrieved)-4,nchar(retrieved)]


> retrieved[nchar(retrieved)-4,nchar(retrieved)]
  5810





s <- read.csv("s.csv")

########running loop to retrieve postcodes form streets in the csv file########


pcsc <- c()
for(i in 1:nrow(empty)){
  
  if(is.na(empty$POSTCODE_AREA[i]) & empty$STREET_NAME[i] != "") {
    
    
    
    pcs <- beera(getposcode(paste0(as.character(empty$STREET_NAME[i]),"+",as.character(empty$SUBURB_NAME[i]))))
    
    if(length(pcs) >0) {
      
      lt <- nchar(pcs$a[1])
      
      postcode <- substr(pcs$a[1],lt-3,lt)
      
      pcsc <- c(postcode,pcsc)
      
      print(pcsc)
      print(paste0(as.character(empty$STREET_NAME[i]),"+",as.character(empty$SUBURB_NAME[i])))
      empty$POSTCODE_AREA[i] <- postcode
    }
    
    pcs <- NULL
  }
  
  
}



