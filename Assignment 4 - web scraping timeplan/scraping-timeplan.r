# Scrape schedule for bed-2056
# By: Sindre Sønvisen (sso149)

library("rvest")
library("tidyverse")

#web url to scrape
url <- "http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list"

#Function to read a html page and return a css section as text.
readHTML <- function(url, css){
  page <- read_html(url)
  HTML <- html_nodes(page, css)
  text <- html_text(HTML)
  return(text)
}

#Use function to get the course titles
entry <- readHTML(url, ".table-primary")

#Parse the stings by \n (newline) and trim unwanted spacing
parsed <- str_split(entry, "\n", simplify = TRUE) %>% trimws()

#insert in to data frame and rename coulombs
df <- parsed[,1:6] %>% data.frame()
names(df) <- c("date", "time", "room", "course", "description", "teacher")

#Fix date,  all week days in norwegian end with g, so just remove everything up to and including "g"
df$date <- gsub(".*g","",df$date) %>% as.Date(format="%d.%m.%y") %>% format(format="%d/%m/%y")

#print
df
