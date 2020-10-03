# Scrape all DataCamp courses for python and R
# By: Sindre Sønvisen (sso149)

library("rvest")
library("tidyverse")

#web url's to scrape
r_url <- "https://www.datacamp.com/courses/tech:r"
python_url <- "https://www.datacamp.com/courses/tech:python"

#Function to read a html page and return a css section as text.
readHTML <- function(url, css){
  page <- read_html(url)
  HTML <- html_nodes(page, css)
  text <- html_text(HTML)
  return(text)
}

#Use function to det the course tiltles
R_course_titles <- readHTML(r_url, ".course-block__title")
python_course_titles <- readHTML(python_url, ".course-block__title")

#insert in data frames
r_df <- data.frame("tech" = R_course_titles, "language" = "R")
python_df <- data.frame("tech" = python_course_titles, "language" = "python")

#print
r_df
python_df
