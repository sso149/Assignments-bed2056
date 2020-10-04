# code from session
# SSB API
# See line 77 for assignment 5

library(PxWebApiData)
library(data.table)
library(tidyverse)

?ApiData

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)


# Use first list, rowbind both data
dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe)
names(dframe) <- c("region", "date", "variable", "value")
str(dframe)

# Split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")
head(dframe)

# Make a new proper date variable
library(lubridate)
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))
str(dframe)

head(dframe)

# And how many levels has the variable?
dframe %>% select(variable) %>% unique()

# or mutate & ifelse, a bit cumbersome, but flexible
dframe <- 
  dframe %>%
  mutate(variable1 =
           ifelse(variable == "Utleigde rom", "rentedrooms",
                  ifelse(variable == "Pris per rom (kr)", "roomprice",
                         ifelse(variable == "Kapasitetsutnytting av rom (prosent)", "roomcap",
                                ifelse(variable == "Kapasitetsutnytting av senger (prosent)", "bedcap",
                                       ifelse(variable == "Losjiomsetning (1 000 kr)", "revenue",
                                              ifelse(variable == "Losjiomsetning per tilgjengeleg rom (kr)", "revperroom",
                                                     ifelse(variable == "Losjiomsetning, hittil i år (1 000 kr)", "revsofar",
                                                            ifelse(variable == "Losjiomsetning per tilgjengeleg rom, hittil i år (kr)", "revroomsofar",
                                                                   ifelse(variable == "Pris per rom hittil i år (kr)", "roompricesofar",
                                                                          ifelse(variable == "Kapasitetsutnytting av rom hittil i år (prosent)", "roomcapsofar", "bedcapsofar")))))))))))


dframe %>% select(variable1) %>% unique()
with(dframe, table(variable, variable1))


# recode region
dframe <- dframe %>% mutate(region = 
                              ifelse(region == "Heile landet",
                                     "Whole country", region))

# And how many regions has the variable?
dframe %>% select(region) %>% unique()

mosaic::tally(~region, data = dframe)

head(dframe)
# we now have the data in long format ready for data wrangling


# Plot room cap over time (------------ Assignment 5 ------------)
# By: Sindre Sønvisen (sso149)
library(ggplot2)

Colors = c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 8))

plot <- subset(dframe,variable1=="roomcap") %>% 
  ggplot(.,aes(x=date , y=value, color=region)) + 
  geom_point() +
  geom_line() +
  scale_color_manual(values=Colors) +
  ggtitle("Room cap for each region") +
  ylab("Room cap") + xlab("Date")

plot
