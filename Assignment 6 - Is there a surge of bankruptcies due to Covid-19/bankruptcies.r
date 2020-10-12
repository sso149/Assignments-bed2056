#Assignment 6 Is there a surge of bankruptcies due to Covid-19?
#By Sindre Sønvisen


bankruptcy <- function(from, to){
  library(tidyverse)
  library(rvest)
  #Parse url
  url <- sprintf("https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=%s&datoTil=%s&id_region=0&id_niva1=51&id_niva2=-+-+-&id_bransje1=0", from, to)
  
  #Get
  html <- read_html(url)
  
  #Select the correct table and remove the five first rows
  xml <- html %>% html_node(xpath = "//*/table[4]") %>% html_node("table") %>% html_table(fill = TRUE) %>% .[c(5,6,8)]
  xml <- xml[-(1:5),]
  
  #Rename
  names(xml) <- c("County", "Dato", "Kunngjøringstype")
  
  #Add County, simple solution where if the cell is empty just take value from previous row.
  for (i in 1:length(xml[,1])){
    if (xml[i,1] == ""){
      xml[i,1] <- xml[i-1,1]
    }
    else{
      #print(xml[i,1])
    }
  }
  
  #Only interested in Kunngjøringstype Konkursåpning
  konkursåpning <- xml[xml$Kunngjøringstype == "Konkursåpning",]
  
  #Convert date
  konkursåpning$Dato <- as.Date(konkursåpning$Dato, format="%d.%m.%Y")
  #Does not care about the day
  konkursåpning$Dato <- format(konkursåpning$Dato, "01.%m.%Y")
  
  #Return
  return(konkursåpning)
  #return(split(konkursåpning, konkursåpning$County))
}

#Helper function to collect larger time frames
helper <- function(y_from, y_to, m_from, m_to){
  #Create empty data frame
  data <- data.frame(matrix(ncol = 3, nrow = 0))
  names(data) <- c("County", "Dato", "Kunngjøringstype")
  
  #Nested for loops over year and month
  for (year in y_from:y_to){
    for (month in m_from:m_to){
      
      #get current date and se compare, return if over
      y <- as.numeric(format(Sys.time(), "%Y"))
      m <- as.numeric(format(Sys.time(), "%m"))
      if (year >= y){
        if(month >= m){
          return(split(data, data$County))
        }
      }
      
      from <- paste("01", formatC(month, width=2, flag="0"), year, sep=".")
      
      #Find to dat based on number in month
      if (month %in% c(1,3,5,7,8,10,12)){
        to <- paste("31", formatC(month, width=2, flag="0"), year, sep=".")
      }
      else if (month %in% c(4,6,9,11)){
        to <- paste("30", formatC(month, width=2, flag="0"), year, sep=".")
      }
      else{ #Feb. may be 29 or 28 based on leap year
        if (lubridate::leap_year(year)){
          to <- paste("29", formatC(month, width=2, flag="0"), year, sep=".")
        }
        else{
          to <- paste("28", formatC(month, width=2, flag="0"), year, sep=".")
        }
      }
      
      #Get data using functuon above
      new <- bankruptcy(from, to)
      #Insert in data df
      data <- rbind(data, new)
    }
  }
  #Return a list with data frames for all Countys
  return(split(data, data$County))
}

#Helper function to plot for all Countys
helper_plot <- function(data){

  Colors = c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 8))
  
  #For loop over the list provided
  for (i in 1:length(data)){
    #Count instances and group by "Dato"
    df <- data.frame(table(unlist(data[[i]]$Dato)))
    df$Var1 <- as.Date(df$Var1, format="%d.%m.%Y")
    
    ##Extract year and month, set year to 0 in month
    df <- df %>% dplyr::mutate(Year = as.factor(lubridate::year(Var1)), Month = as.Date(format(df$Var1, "0-%m-%d")))
    
    #Find county to use as title
    County <- sprintf("County: %s", data[[i]]$County)
  
    #Create plot
    plot <- df %>% 
      ggplot(.,aes(x=Month , y=as.numeric(Freq), color=Year)) + 
      geom_point() +
      geom_line() +
      scale_color_manual(values=Colors) +
      ggtitle(County) +
      ylab("number of cases") + xlab("Month")
    #Show plot
    print(plot)
  }
}

#Example
#test <- helper(2019,2020,1,12)
#test

#example
#helper_plot(test)

#Example usage
#konkursåpning <- bankruptcy("01.01.2019", "31.01.2019")
#konkursåpning
