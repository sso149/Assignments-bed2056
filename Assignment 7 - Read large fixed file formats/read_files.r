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
              fwf_positions(start = c(475, 504, 23, 13, 9),
                            end = c(475, 507, 23, 14, 12),
                            col_names = c("sex", "weight", "week_day", "month", "year")
                            )
              )
  #Combine all the data to one data frame
  df <- bind_rows(list)
  #Weight shuld be numeric
  df <- mutate(df, weight=as.numeric(weight))
  #Make a new field with the day names.
  days <- c("Sun", "Mon", "Tues", "Wed", "Thur", "Fri", "Sat")
  df <- mutate(df, weekday=factor(days[week_day]))
  
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df <- mutate(df, month=as.numeric(month)) %>% mutate(month_n=factor(months[month]))
  
  #Rename F to Female and M to Male
  df <- df %>% 
    mutate(sex =
           ifelse(sex == "F", "Female",
                  ifelse(sex == "M", "Male", "unknown")))
  return(df)
}

#The data need to be unzipped and placed in a folder named data, or change the loc and or pattern argument bellow.
df <- read_files(loc="./data", pattern=".txt")

# bar graph of proportion between genders for each month each year.
graph_sex <- 
  ggplot(df, aes(x = reorder(month_n, month), group=sex, fill=sex)) +
  geom_bar(stat = "count",
            width = 0.7,
            position = "fill") +
  facet_wrap( ~ year) +
  ggtitle("Proportion of girls and boys born each month") +
  ylab("Proportion Male Female") + xlab("Month") +
  labs(color = "Gender per year")

# line graph of average weight for both genders.
graph_weight_by_sex <-
  ggplot(df[df$weight != 9999,], aes(x = reorder(month_n, month), y = weight, group=interaction(sex,year), color=interaction(sex,year))) +
  geom_line(stat = "summary", fun=mean) +
  ggtitle("weight by gender each month") +
  ylab("Average weight in grams") + xlab("Month") +
  labs(color = "Gender per year")

# bar graph of genders for each week day
graph_sex_by_weekday <-
  ggplot(df, aes(x = sex, group=reorder(weekday, week_day), fill=reorder(weekday, week_day))) +
  geom_bar(stat = "count",
           width = 0.7,
           position = "fill"
           ) +
  facet_wrap( ~ year) +
  ggtitle("Gender proportion by Weekday") +
  ylab("Proportion Weekday") + xlab("Gender") +
  labs(fill = "Weekday")

