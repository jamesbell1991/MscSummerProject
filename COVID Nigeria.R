# This script plots COVID cases over time in Nigeria 

# Required packages 
library(dplyr)
library(tidyr)
library(ggplot2)
library(covid19.analytics)
library(lubridate)

# Retrieve data using covid19.analytics package (Hopkins data)
coronavirus<- covid19.data("ts-confirmed")


# Filter to Nigeria 
NG <- coronavirus %>% 
  filter(Country.Region == "Nigeria")

# Reshape data 
NG <- pivot_longer(
  NG, 
  "2020-01-22":"2020-12-20", 
  names_to = "Date"
)

# Convert dates
NG$Date<-as.Date(NG$Date)

# Plot 
ggplot(
  data = NG, 
  aes(x=Date, 
      y=value, 
      group=1))+
  geom_line(colour = "#6E7677", size = 1)+
  theme_bw()+
  xlab("Date")+
  ylab("Number of cases")+ 
  labs(title = "Figure 1: Cumulative Reported COVID Cases in Nigeria",
       caption = "Data source: Johns Hopkins")+ 
  xlim(as.Date(c('1/1/2020', '1/1/2021'), format="%d/%m/%Y") )
  