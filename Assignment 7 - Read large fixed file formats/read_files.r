#Bed-2056 assignment-7 by Sindre Sønvisen (sso149)
#This file contains the function to read the data for this assignment in to a data frame

library(tidyverse)

#Function to read large data sets in a directory.
#Note: The position for the data in the files is hard coded in the function.
# loc: location of the data, default to current directory.
# pattern: pattern to look for in the file names (example ".txt" will only use all the text files).
read_files <- function(loc=".", pattern=""){
  #Get all the files we want in a list (with full path)
  files = list.files(path=loc, pattern=pattern, full.names=TRUE)
  #Uses map from tidyverse to read the data from all files
  #Map uses read_fwf tidyverse to actually read the fixed format files.
  list <- map(files, read_fwf,
              fwf_positions(start = c(475, 504, 23),
                            end = c(475, 507, 23),
                            col_names = c("sex", "weight", "week_day")
                            )
              )
  #Combine all the data to one data frame
  df <- bind_rows(list)
  #Weight shuld be numeric
  df <- mutate(df, weight=as.numeric(weight))
  #Rename F to Female and M to Male
  df <- df %>% 
    mutate(sex =
           ifelse(sex == "F", "Female",
                  ifelse(sex == "M", "Male", "unknown")))
  return(df)
}

