#Assignment 6 Is there a surge of bankruptcies due to Covid-19?
#By Sindre Sønvisen

library(tidyverse)
library(rvest)

bankruptcy <- function(from, to){
  #Parse url
  url <- sprintf("https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=%s&datoTil=%s&id_region=0&id_niva1=51&id_niva2=-+-+-&id_bransje1=0", from, to)
  
  #Get
  html <- read_html(url)
  
  #Select the correct table and remove the five first rows
  xml2 <- html %>% html_node(xpath = "//*/table[4]") %>% html_node("table") %>% html_table(fill = TRUE) %>% .[c(5,6,8)]
  xml3 <- xml2[-(1:5),]
  head(xml3, 10)
  
  #Rename
  names(xml3) <- c("Region", "Dato", "Kunngjøringstype")
  head(xml3)
  
  #Add region
  for (i in 1:length(xml3[,1])){
    if (xml3[i,1] == ""){
      xml3[i,1] <- xml3[i-1,1]
    }
    else{
      print(xml3[i,1])
    }
  }
  
  #Only interested in Kunngjøringstype Konkursåpning
  konkursåpning <- xml3[xml3$Kunngjøringstype == "Konkursåpning",]
  head(konkursåpning)
  
  #Split by region and return
  return(split(konkursåpning, konkursåpning$Region))
}

#Example usage
konkursåpning <- bankruptcy("01.01.2019", "31.01.2019")
konkursåpning
