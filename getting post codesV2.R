
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


##


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


count<-0


####detect if postcodes are within cityname and other address columns:
for(i in 1:nrow(empty)){
  
  if (any(str_detect(as.character(empty$CITY_NAME[i]),"[0-9]"))) {
    
    count <- count +1
    
    
    #print(as.character(empty$CITY_NAME[i]))
    
    last <- length(strsplit(as.character(empty$CITY_NAME[i])," ")[[1]])
    
    postcode <- strsplit(as.character(empty$CITY_NAME[i])," ")[[1]][last]
    
    
    
    
    empty$POSTCODE_AREA[i] <- postcode

    print(postcode)
      
      
      
      
      
  }
  
  
}



##########
count <- 0
for(i in 1:nrow(empty)){
  
  
  #
  ca <- nchar(as.character(empty$ADDRESS_LINE_ONE[i]))
  cs <- substr(as.character(empty$ADDRESS_LINE_ONE[i]),ca-3,ca)
  
  
  if (any(str_detect(cs,"[0-9]{4}"))) {
    
    count <- count +1
    
    
    #print(as.character(empty$CITY_NAME[i]))
    
    #last <- length(strsplit(as.character(empty$ADDR_LINE_2[i])," ")[[1]])
    
    #postcode <- strsplit(as.character(empty$ADDR_LINE_2[i])," ")[[1]][last]
    postcode <- str_trim(cs)
    
    
    
    empty$POSTCODE_AREA[i] <- postcode
    
    print(postcode)
    print(i)
    
  }
  
  
}











  
s <- read.csv("s.csv")
  
########screening stree_name column####
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









length(which(empty$ADDRESS_LINE_ONE != "" & is.na(empty$POSTCODE_AREA) & empty$ADDR_LINE_2 != "" & empty$CITY_NAME != ""))

########scraping addr_line column####
pcsc <- c()
pcss <- c()
for(i in 1:nrow(empty)){
print(i)
if((is.na(empty$POSTCODE_AREA[i]) & empty$ADDRESS_LINE_ONE[i] != "" &  empty$ADDR_LINE_2[i] != "" & empty$CITY_NAME[i] != "")) {
fads <- str_trim(gsub('[[:digit:]]+','',as.character(empty$ADDRESS_LINE_ONE[i])))
fads <- str_trim(gsub(' ','+',fads))
fads2 <- str_trim(gsub('[[:digit:]]+','',as.character(empty$ADDR_LINE_2[i])))
fads2 <- str_trim(gsub(' ','+',fads2))
fads3 <- str_trim(gsub('[[:digit:]]+','',as.character(empty$CITY_NAME[i])))
fads3 <- str_trim(gsub(' ','+',fads3))
#pcs <- beera(getposcode(paste0(fads,"+",fads3)))
pcs <- beera(getposcode(paste0(fads,"+",fads2,"+",fads3)))
if(length(pcs) >0 & !grepl("DO NOT",toupper(paste0(as.character(empty$ADDRESS_LINE_ONE[i],as.character(empty$CITY_NAME[i])))) ) & !grepl("POBOX",toupper(gsub(" ","",str_trim(paste0(as.character(empty$ADDRESS_LINE_ONE[i],as.character(empty$CITY_NAME[i]))))) ))) {
lt <- nchar(pcs$a[1])
postcode <- substr(pcs$a[1],lt-3,lt)
pcsc <- c(postcode,pcsc)
pcss <- c(as.character(empty$ADDRESS_LINE_ONE[i]),pcss)
#print(pcsc)
#print(paste0(as.character(empty$STREET_NAME[i]),"+",as.character(empty$SUBURB_NAME[i])))
#print(paste0(fads,"+",fads2,"+",fads3))
empty$POSTCODE_AREA[i] <- postcode
}
pcs <- NULL
}
}




write_csv(empty,"empty",na="NA")







install.packages("tm")
install.packages("hrbrthemes")
install.packages("tm")
install.packages("proustr")
install.packages("VennDiagram")
library(tidyverse)
library(hrbrthemes)
library(tm)
library(proustr)

library(VennDiagram)




venn.diagram(
  x = list(
    data %>% filter(artist=="booba") %>% select(word) %>% unlist() , 
    data %>% filter(artist=="nekfeu") %>% select(word) %>% unlist() , 
    data %>% filter(artist=="georges-brassens") %>% select(word) %>% unlist()
  ),
  category.names = c("Dion (1995)" , "Ken (663)" , "Sheema (471)"),
  filename = 'venn.png',
  output = TRUE ,
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
  rotation = 1
)






c$routes[[1]]$legs[[1]]$start_location
lat       lng 
-36.89363 174.77438 

> 
  
  
  
  
  
c$routes[[1]]$legs[[1]]$start_location


  
###########getting long lat#####################

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "https://maps.googleapis.com/maps/api/directions/"
  address <- str_trim(gsub(' ','+',address))
  u <- paste(root, return.call, "?origin=NZ+", address, "&destination=NZ+", address,"&key=AIzaSyBtkIbZc_AtjowFEH5R2ZbYLknacwLclzE " ,sep = "")
  print(u)
  return(getURL(u))
}
  
  


geoCode <- function(address,verbose=FALSE) {
  #if(verbose) cat(address,"\n")
  u <- url(address)
  if(verbose) cat(u,"\n")
  #doc <- getURL(u)
  x <- fromJSON(u)
  if(x$status=="OK") {
    lat <- x$routes[[1]]$legs[[1]]$start_location[[1]]
    lng <- x$routes[[1]]$legs[[1]]$start_location[[2]]
   # location_type <- x$results[[1]]$geometry$location_type
   # formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng))
  } else {
    return(c(NA,NA,NA, NA))
  }
}
###################################################################

  
as <- "https://maps.googleapis.com/maps/api/directions/json?origin=Auckland+Epsom&destination=Auckland+new+market&key=AIzaSyDd1krlJyE2tQeByO551tVCbr2hVAvxLtw"
  
  
  



